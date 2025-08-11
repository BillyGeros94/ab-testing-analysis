# A/B Testing Analysis — Landing Page Experiment

**Recommendation (TL;DR):** **Hold — do not roll out the treatment.**\
Treatment conversion is slightly lower than control and the data do not support a practical or statistical improvement.

## Key numbers (quick view)

| metric                          | value                       |
| ------------------------------- | --------------------------- |
| n\_control                      | `147,202`                   |
| n\_treatment                    | `147,276`                   |
| conv\_rate\_control             | `0.1204` (12.04%)           |
| conv\_rate\_treatment           | `0.1189` (11.89%)           |
| delta (treatment − control)     | `-0.0015` (−0.15 pp)        |
| two-sided p-value (z-test)      | `0.1898`                    |
| Bayesian P(Treatment > Control) | `0.0946`                    |
| 95% CI for Δ                    | `[-0.0039379, 0.0007814]`   |
| experiment dates                | `2017-01-02` → `2017-01-24` |
| conversion window               | `see full report`           |

---

## One-line justification

Frequentist test: no statistically significant lift (p ≈ 0.19). Bayesian posterior gives \~9.5% probability the treatment is better. Confidence and credible intervals include zero and are centered slightly negative — do not roll out.

---

## How to reproduce (one command)

From the project root:

```bash
Rscript src/main.R
```

Expected outputs: `report/Project - Report.pdf` and `presentation/Project-Presentation.pptx`.

---

## Minimal methods summary

- **Data:** `data/ab_data.csv` (user-level A/B events) and `data/countries.csv` (user → country).
- **Cleaning:** Mismatches removal; Deduplication kept the **earliest** exposure per `user_id` (based on POSIX timestamps); join to countries performed after cleaning. No sample loss after the cleaning/join step.
- **Frequentist:** two-sided z-test for difference in proportions (p-value and 95% CI reported).
- **Bayesian:** independent Beta posterior per group with Beta(1,1) prior; posterior samples used to compute P(Treatment > Control).
- **Segmentation:** per-country analyses performed (decisions treated as country-specific where sample sizes justify it).

---

## Assumptions & decisions (short)

- Fixed-horizon experiment — no sequential/peeking corrections applied.
- Deduplication rule: earliest timestamp retained per user.
- Conversion attribution window and business-specific adjustments are documented in `report/Project - Report.pdf`.
- Priors: Beta(1,1) used for clarity; no prior sensitivity analysis is in the headline results.

---

## Limitations

- Conversion-only analysis: revenue/LTV not included — perform revenue analysis to assess business impact.
- Country-level estimates for small samples are noisy; treat small-country results with caution.
- No sequential testing correction: if the experiment was monitored and stopped early, reported p-values are optimistic.

---

## Recommended next steps

1. Run revenue-per-user / LTV analysis to measure business impact.
2. If pursuing a rollout for a subgroup, run a follow-up pre-registered experiment with a clear MDE and fixed horizon.

---

## Project structure (short)

- `data/` — source CSVs (`ab_data.csv`, `countries.csv`)
- `src/` — analysis scripts (`main.R`, `frequentist.R`, `bayesian.R`, `data_prep.R`, etc.)
- `report/` — full analytical report (PDF) with detailed methods, checks, and plots
- `presentation/` — slide deck summarizing findings

---