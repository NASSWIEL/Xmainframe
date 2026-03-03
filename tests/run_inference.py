"""
XMAiNframe SLM Inference Test
==============================================
Tests the XMAiNframe-instruct-7b model against
our COBOL test programs across all 3 evaluation
tasks: Summarization, QA, and Multiple Choice.

README inference pattern:
  messages = [{'from': 'system', 'value': ...},
              {'from': 'human', 'value': ...}]
  inputs = tokenizer.apply_chat_template(messages,
               add_generation_prompt=True, return_tensors="pt")
  outputs = model.generate(inputs, max_new_tokens=512, ...)
"""

import os
import sys
import time
import textwrap
import traceback
import torch
import psutil

COBOL_DIR = os.path.join(os.path.dirname(__file__), "cobol")
MODEL_ID   = "Fsoft-AIC/XMAiNframe-instruct-7b"
SYSTEM_MSG = "You are an expert mainframe and COBOL assistant."

# ── colour helpers ──────────────────────────────────────────────────────────

def banner(title: str, width: int = 70) -> None:
    print("\n" + "=" * width)
    print(f"  {title}")
    print("=" * width)

def section(title: str) -> None:
    print(f"\n{'─' * 60}")
    print(f"  {title}")
    print("─" * 60)

def wrap(text: str, indent: int = 4) -> str:
    prefix = " " * indent
    return "\n".join(
        textwrap.fill(line, width=80, initial_indent=prefix,
                      subsequent_indent=prefix)
        for line in text.splitlines()
    )

# ── resource check ──────────────────────────────────────────────────────────

def check_resources() -> dict:
    mem   = psutil.virtual_memory()
    has_cuda  = torch.cuda.is_available()
    vram_gb   = 0.0
    gpu_name  = "N/A"
    if has_cuda:
        props    = torch.cuda.get_device_properties(0)
        vram_gb  = props.total_memory / 1024**3
        gpu_name = props.name

    return {
        "total_ram_gb": mem.total / 1024**3,
        "avail_ram_gb": mem.available / 1024**3,
        "has_cuda":     has_cuda,
        "vram_gb":      vram_gb,
        "gpu_name":     gpu_name,
    }

def print_resource_summary(r: dict) -> None:
    banner("HARDWARE AVAILABILITY CHECK")
    print(f"  Total RAM  : {r['total_ram_gb']:.1f} GB")
    print(f"  Free  RAM  : {r['avail_ram_gb']:.1f} GB")
    print(f"  CUDA GPU   : {'✓ ' + r['gpu_name'] if r['has_cuda'] else '✗  Not available'}")
    if r["has_cuda"]:
        print(f"  VRAM       : {r['vram_gb']:.1f} GB")
    # Model requirements
    print()
    print("  XMAiNframe-7b requirements:")
    print("    float16  → ~14 GB  (GPU VRAM or RAM)")
    print("    float32  → ~28 GB  (RAM)")
    print("    4-bit    →  ~4 GB  (GPU VRAM, CUDA only)")
    can_gpu  = r["has_cuda"] and r["vram_gb"] >= 14
    can_cpu  = r["avail_ram_gb"] >= 14
    print()
    if can_gpu:
        print("  ✓  GPU inference: POSSIBLE")
    elif can_cpu:
        print("  ✓  CPU inference (slow): POSSIBLE")
    else:
        print("  ✗  Insufficient resources for local inference.")
        print("     Showing prompt structure only (tokenizer still runs).")

# ── load COBOL source files ──────────────────────────────────────────────────

def load_cobol(filename: str) -> str:
    path = os.path.join(COBOL_DIR, filename)
    with open(path, encoding="utf-8") as f:
        return f.read()

# ── test definitions ─────────────────────────────────────────────────────────

def build_test_cases() -> list[dict]:
    return [
        # --- Task 1: COBOL Code Summarization ---
        {
            "task":    "Summarization",
            "file":    "HELLO.cbl",
            "prompt":  (
                "Summarize the following COBOL program and explain what it does:\n\n"
                + load_cobol("HELLO.cbl")
            ),
        },
        {
            "task":    "Summarization",
            "file":    "PAYROLL.cbl",
            "prompt":  (
                "Provide a detailed summary of the following COBOL payroll program, "
                "describing its main logic sections, data structures, and calculations:\n\n"
                + load_cobol("PAYROLL.cbl")
            ),
        },
        {
            "task":    "Summarization",
            "file":    "BANKTXN.cbl",
            "prompt":  (
                "Summarize this COBOL banking transaction program. "
                "What transaction types does it support and how does error handling work?\n\n"
                + load_cobol("BANKTXN.cbl")
            ),
        },

        # --- Task 2: Question Answering ---
        {
            "task":    "Question Answering",
            "file":    "CUSTMGMT.cbl",
            "prompt":  (
                "Given the following COBOL program, answer this question:\n"
                "How does the program handle the case where a customer's balance "
                "exceeds their credit limit? Refer to specific paragraph names.\n\n"
                + load_cobol("CUSTMGMT.cbl")
            ),
        },
        {
            "task":    "Question Answering",
            "file":    "INVNTORY.cbl",
            "prompt":  (
                "Given the following COBOL program, answer this question:\n"
                "Explain the difference between how SEARCH and SEARCH ALL are used "
                "in this program, and why each was chosen for its specific table.\n\n"
                + load_cobol("INVNTORY.cbl")
            ),
        },
        {
            "task":    "Question Answering",
            "file":    "BATCHJCL.cbl",
            "prompt":  (
                "Given the following COBOL program, answer this question:\n"
                "What validations are performed at end-of-file and what RETURN-CODE "
                "values can the program set? Explain each condition.\n\n"
                + load_cobol("BATCHJCL.cbl")
            ),
        },

        # --- Task 3: Multiple Choice Questions ---
        {
            "task":    "Multiple Choice",
            "file":    "N/A",
            "prompt":  (
                "Answer the following multiple-choice question about COBOL. "
                "Select the best answer and briefly explain why.\n\n"
                "Question: In a COBOL program, what is the purpose of the COMP-3 "
                "usage clause on a numeric field?\n\n"
                "A) It stores the number in EBCDIC display format\n"
                "B) It stores the number in packed decimal (BCD) format, "
                "saving storage space\n"
                "C) It stores the number as a standard binary integer\n"
                "D) It stores the number using floating-point IEEE 754 format\n"
            ),
        },
        {
            "task":    "Multiple Choice",
            "file":    "N/A",
            "prompt":  (
                "Answer the following multiple-choice question about COBOL "
                "mainframe batch processing. Select the best answer and explain.\n\n"
                "Question: In JCL-driven COBOL batch processing, what is the "
                "primary purpose of checking the RETURN-CODE (condition code) "
                "at the JCL step level?\n\n"
                "A) To get the number of records processed by the COBOL program\n"
                "B) To determine whether to execute subsequent steps based on "
                "success or failure of the current step\n"
                "C) To estimate the elapsed CPU time of the step\n"
                "D) To set the printer output class for report generation\n"
            ),
        },
    ]

# ── inference ───────────────────────────────────────────────────────────────

def run_inference(model, tokenizer, test_case: dict, device: str) -> str:
    messages = [
        {"from": "system", "value": SYSTEM_MSG},
        {"from": "human",  "value": test_case["prompt"]},
    ]
    inputs = tokenizer.apply_chat_template(
        messages,
        add_generation_prompt=True,
        return_tensors="pt",
    ).to(device)

    with torch.no_grad():
        outputs = model.generate(
            inputs,
            max_new_tokens=400,
            do_sample=False,
            top_k=50,
            top_p=0.95,
            num_return_sequences=1,
            eos_token_id=tokenizer.eos_token_id,
            pad_token_id=tokenizer.eos_token_id,
        )

    response = tokenizer.decode(
        outputs[0][len(inputs[0]):],
        skip_special_tokens=True,
    )
    return response.strip()


def show_prompt_only(tokenizer, test_case: dict) -> None:
    """When no model can be loaded, show what the tokenised prompt looks like."""
    messages = [
        {"from": "system", "value": SYSTEM_MSG},
        {"from": "human",  "value": test_case["prompt"]},
    ]
    formatted = tokenizer.apply_chat_template(
        messages,
        add_generation_prompt=True,
        tokenize=False,
    )
    # Show first 600 chars of the formatted prompt
    preview = formatted[:600] + ("…" if len(formatted) > 600 else "")
    print(f"\n  [Formatted prompt sent to model — first 600 chars]\n")
    for line in preview.splitlines():
        print(f"    {line}")


# ── main ─────────────────────────────────────────────────────────────────────

def main() -> None:
    banner("XMAiNframe SLM — COBOL INFERENCE TEST SUITE")

    res = check_resources()
    print_resource_summary(res)

    can_gpu  = res["has_cuda"] and res["vram_gb"] >= 14
    can_cpu  = res["avail_ram_gb"] >= 14
    can_run  = can_gpu or can_cpu

    # ── load tokenizer (always works, ~500 MB download) ────────────────────
    banner("LOADING TOKENIZER")
    print(f"  Model: {MODEL_ID}")
    try:
        from transformers import AutoTokenizer, AutoModelForCausalLM
        print("  Downloading/loading tokenizer …")
        t0 = time.time()
        tokenizer = AutoTokenizer.from_pretrained(MODEL_ID, trust_remote_code=True)
        print(f"  ✓ Tokenizer loaded in {time.time()-t0:.1f}s")
        vocab = tokenizer.vocab_size
        print(f"  Vocabulary size : {vocab:,}")
        print(f"  Chat template   : {'✓ Present' if tokenizer.chat_template else '✗ Missing'}")
    except Exception as exc:
        print(f"  ✗ Tokenizer load failed: {exc}")
        sys.exit(1)

    # ── optionally load model ──────────────────────────────────────────────
    model  = None
    device = "cpu"

    if can_run:
        banner("LOADING MODEL")
        dtype  = torch.float16
        device = "cuda" if can_gpu else "cpu"
        print(f"  Device     : {device.upper()}")
        print(f"  dtype      : float16")
        print(f"  Loading {MODEL_ID} …  (this may take several minutes)")
        try:
            t0 = time.time()
            model = AutoModelForCausalLM.from_pretrained(
                MODEL_ID,
                torch_dtype=dtype,
                device_map="auto",
                low_cpu_mem_usage=True,
                trust_remote_code=True,
            )
            model.eval()
            elapsed = time.time() - t0
            params  = sum(p.numel() for p in model.parameters()) / 1e9
            print(f"  ✓ Model loaded in {elapsed:.1f}s  ({params:.2f}B parameters)")
        except Exception as exc:
            print(f"  ✗ Model load failed: {exc}")
            print("    Falling back to prompt-structure demo.")
            model = None
    else:
        banner("MODEL LOAD SKIPPED")
        print("  Insufficient RAM/VRAM. Showing tokenised prompt structure instead.")
        print("  To run generation you need one of:")
        print("    • A CUDA GPU with ≥14 GB VRAM (float16)")
        print("    • ≥14 GB free system RAM (CPU, very slow)")
        print("    • A CUDA GPU with ≥4 GB VRAM + bitsandbytes 4-bit quant")

    # ── run test cases ─────────────────────────────────────────────────────
    test_cases = build_test_cases()
    banner(f"RUNNING {len(test_cases)} TEST CASES")

    for i, tc in enumerate(test_cases, 1):
        section(f"Test {i}/{len(test_cases)}  │  Task: {tc['task']}  │  File: {tc['file']}")

        # Print a preview of the prompt
        prompt_preview = tc["prompt"][:300].replace("\n", " ").strip()
        print(f"\n  Prompt preview: «{prompt_preview}…»\n")

        if model is not None:
            try:
                t0 = time.time()
                response = run_inference(model, tokenizer, tc, device)
                elapsed  = time.time() - t0
                print(f"  ── Model response ({elapsed:.1f}s) ──")
                print()
                print(wrap(response))
                print()
            except Exception as exc:
                print(f"  ✗ Generation failed: {exc}")
                traceback.print_exc()
        else:
            show_prompt_only(tokenizer, tc)

    banner("TEST SUITE COMPLETE")
    if model is None:
        print()
        print("  NOTE: To see real model output, run on a machine with:")
        print("    • CUDA GPU ≥14 GB VRAM  (recommended)")
        print("    • Or upgrade RAM to ≥16 GB free  (CPU-only, ~10–30 min/response)")
        print()
        print("  The tokenizer + prompt structure above is exactly what the model")
        print("  would receive. The apply_chat_template output confirms the")
        print("  DeepSeek chat format is correctly applied to each COBOL test.")


if __name__ == "__main__":
    main()
