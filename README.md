# score2pexa

**SCORE2** (ages 40–69) and **SCORE2-OP** (ages 70–89) as a ModelsCloud
service: the ESC's 10-year risk of fatal and non-fatal cardiovascular disease,
recalibrated to four European risk regions.

SCORE2 working group. *Eur Heart J.* 2021;42(25):2439–2454.
[doi:10.1093/eurheartj/ehab309](https://doi.org/10.1093/eurheartj/ehab309) ·
SCORE2-OP working group. *Eur Heart J.* 2021;42(25):2455–2467.
[doi:10.1093/eurheartj/ehab312](https://doi.org/10.1093/eurheartj/ehab312)

## Inputs

| Field | Coding |
|---|---|
| `age` | 40–89 (SCORE2-OP used from 70) |
| `sex` | `"male"` / `"female"` |
| `smoking` | current smoker, yes / no |
| `sbp` | systolic BP, mmHg |
| `total_chol`, `hdl` | mmol/L, or set `chol_units = "mg/dL"` |
| `diabetes` | yes / no (default no) |
| `region` | `"low"`, `"moderate"`, `"high"`, `"very_high"` |

Returns `model` (SCORE2 or SCORE2-OP) and `risk` on 0–1.

For people with diabetes under 70, the ESC recommends SCORE2-Diabetes instead.

## Validation

Matches MDCalc's SCORE2 and SCORE2-OP calculators on 16 patients covering
both sexes, all four regions and ages 40–85, within their 1-decimal display.
An independent implementation (`RiskScorescvd`) agrees on all 16.

## Use

```r
remotes::install_git("https://github.com/raminrzn/score2pexa", ref = "main")
score2pexa::model_run(list(age = 50, sex = "male", smoking = "no", sbp = 140,
                           total_chol = 6.3, hdl = 1.4, region = "moderate"))
```

For research use. Not a medical device.

## License

GPL-3.
