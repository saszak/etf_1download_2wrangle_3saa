---
name: FMP analyst targets
description: Pending task — fetch live 12M analyst price targets via FMP API for Top Picks treemap
type: project
---

User needs a free FMP API key (financialmodelingprep.com, 250 req/day free tier).

Once key is available, build `fetch_analyst_targets(tickers, api_key)`:
- Endpoint: `/price-target-consensus` (FMP)
- Returns: targetConsensus, targetHigh, targetLow, publishedDate
- Compute: upside_pct = (targetConsensus / currentPrice) - 1
- Cache to: `input/analyst_targets.rds` (refresh daily)
- Feed into: Top Picks treemap in `mod_treemap.R` to replace static Excel values

**Why:** Yahoo Finance now requires crumb auth + rate limits; FMP free tier is the cleanest alternative.
**How to apply:** When user mentions FMP key or analyst targets, build the fetch utility and wire it into the treemap.
