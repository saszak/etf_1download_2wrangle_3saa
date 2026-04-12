# ==============================================================================
# shiny_dashboard/modules/mod_patterns.R
# PURPOSE : "Patterns" — 5 analytical lenses on monthly market behaviour
#   1. Surprise   — cross-sectional z-score of relative returns vs SPY
#   2. State Flip — 200DMA crossings vs 22 trading days ago
#   3. Rotation   — Relative Rotation Graph (RS-Ratio × RS-Momentum)
#   4. Corr Shift — 1M vs 3M correlation matrix delta
#   5. Analogue   — cosine similarity to historical 1M return profiles
# ==============================================================================

.PAT_WIN <- c("1W" = 5L, "1M" = 22L, "1Q" = 63L, "1Y" = 252L)

.pat_hdr <- function(...) {
  div(style = paste0(
    "background:", DARK_HDR, "; color:", TEXT_DIM, ";",
    " font-size:11px; padding:5px 12px; margin-bottom:4px;",
    " display:flex; gap:16px; align-items:center;"
  ), ...)
}

.empty_plot <- function(msg = "No data") {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = msg,
             colour = "#7f8c8d", size = 4) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#f8f9fa", colour = NA))
}

.pat_light_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background    = element_rect(fill = "#f8f9fa", colour = NA),
      panel.background   = element_rect(fill = "#f8f9fa", colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#e8eaed", linewidth = 0.4),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(colour = "#1B3A6B", face = "bold", size = 9),
      axis.title         = element_text(colour = "#1B3A6B", face = "bold", size = 9),
      plot.title         = element_text(colour = "#1c2833", face = "bold", size = 12),
      plot.subtitle      = element_text(colour = "#1B3A6B", size = 10),
      plot.margin        = margin(8, 16, 8, 8)
    )
}

# ── UI ─────────────────────────────────────────────────────────────────────────
mod_patterns_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    type = "pills",

    # 1. Surprise
    tabPanel("Surprise",
      tagList(
        .pat_hdr(
          span("Z-score of cumulative relative return vs SPY  \u2502  \u00b12\u03c3 = historically unusual"),
          div(style = "margin-left:auto; display:flex; align-items:center; gap:8px;",
            span("Window:"),
            radioButtons(ns("surp_win"), NULL,
              choices  = c("1W","1M","1Q","YTD","1Y"),
              selected = "1M", inline = TRUE))
        ),
        layout_columns(col_widths = c(7, 5),
          div(style = "overflow-y:auto; height:calc(100vh - 302px); background:#f8f9fa;",
              plotOutput(ns("surp_plot"), height = "calc(100vh - 302px)")),
          div(style = paste0("overflow-y:auto; height:calc(100vh - 302px); background:", DARK_PANEL, ";"),
              reactableOutput(ns("surp_tbl")))
        )
      )
    ),

    # 2. State Flip
    tabPanel("State Flip",
      tagList(
        .pat_hdr(
          span("200DMA signal vs 22 trading days ago  \u2502  \u2605 = crossed this month  \u2502  Sorted by distance to MA200")
        ),
        layout_columns(col_widths = c(5, 7),
          div(style = paste0("overflow-y:auto; height:calc(100vh - 302px); background:", DARK_PANEL, ";"),
              reactableOutput(ns("flip_tbl"))),
          div(style = "overflow-y:auto; height:calc(100vh - 302px); background:#f8f9fa;",
              plotOutput(ns("flip_plot"), height = "calc(100vh - 302px)"))
        )
      )
    ),

    # 3. Rotation
    tabPanel("Rotation",
      tagList(
        .pat_hdr(
          span("Relative Rotation Graph  \u2502  RS-Ratio \u00d7 RS-Momentum  \u2502  Trail = last 4 weekly snapshots"),
          div(style = "margin-left:auto; display:flex; align-items:center; gap:8px;",
            span("RS window:"),
            radioButtons(ns("rrg_win"), NULL,
              choices  = c("1M" = "22", "1Q" = "63", "6M" = "126"),
              selected = "63", inline = TRUE))
        ),
        div(style = "overflow-y:auto; height:calc(100vh - 302px); background:#f8f9fa;",
            plotOutput(ns("rrg_plot"), height = "calc(100vh - 302px)"))
      )
    ),

    # 4. Momentum
    tabPanel("Momentum",
      tagList(
        .pat_hdr(
          span("YTD vs 1M performance scatter  \u2502  Quadrants: Leading / Improving / Weakening / Lagging")
        ),
        div(style = "overflow-y:auto; height:calc(100vh - 302px); background:#f8f9fa;",
            plotOutput(ns("mom_plot"), height = "calc(100vh - 302px)"))
      )
    ),

    # 5. Corr Shift
    tabPanel("Corr Shift",
      tagList(
        .pat_hdr(
          span("1M correlation minus 3M baseline  \u2502  Blue = recoupled  \u2502  Red = decoupled  \u2502  |\u0394\u03c1| > 0.3 flagged")
        ),
        layout_columns(col_widths = c(7, 5),
          div(style = "overflow-y:auto; height:calc(100vh - 302px); background:#f8f9fa;",
              plotOutput(ns("corr_plot"), height = "calc(100vh - 302px)")),
          div(style = paste0("overflow-y:auto; height:calc(100vh - 302px); background:", DARK_PANEL, ";"),
              reactableOutput(ns("corr_tbl")))
        )
      )
    ),

    # 6. Analogue
    tabPanel("Analogue",
      tagList(
        .pat_hdr(
          span("Cosine similarity of current 1M return profile to all historical 1M windows  \u2502  Top 10 nearest neighbours")
        ),
        layout_columns(col_widths = c(5, 7),
          div(style = paste0("overflow-y:auto; height:calc(100vh - 302px); background:", DARK_PANEL, ";"),
              reactableOutput(ns("ana_tbl"))),
          div(style = "overflow-y:auto; height:calc(100vh - 302px); background:#f8f9fa;",
              plotOutput(ns("ana_plot"), height = "calc(100vh - 302px)"))
        )
      )
    )
  )
}

# ── Server ─────────────────────────────────────────────────────────────────────
mod_patterns_server <- function(id, filtered) {
  moduleServer(id, function(input, output, session) {

    syms_r <- reactive(filtered()$symbol)

    # =========================================================================
    # 1. SURPRISE
    # =========================================================================
    surp_df <- reactive({
      win  <- if (is.null(input$surp_win)) "1M" else input$surp_win
      n    <- if (win == "YTD") sum(zoo::index(xts_rel_shiny) >= ytd_start) else .PAT_WIN[[win]]
      n    <- max(n, 1L)
      syms <- syms_r(); syms <- syms[syms %in% colnames(xts_rel_shiny) & syms != "SPY"]
      if (!length(syms) || nrow(xts_rel_shiny) < n * 2L) return(tibble())

      mat <- as.matrix(xts_rel_shiny[, syms, drop = FALSE])
      nr  <- nrow(mat)

      bind_rows(lapply(syms, function(tk) {
        x <- mat[, tk]; x[is.na(x)] <- 0
        rc  <- vapply(n:nr, function(i) sum(x[(i-n+1L):i]), numeric(1L))
        cur <- rc[length(rc)]; hv <- rc[-length(rc)]
        mu  <- mean(hv, na.rm=TRUE); sg <- sd(hv, na.rm=TRUE)
        tibble(ticker=tk, cum_rel=cur*100, hist_mean=mu*100, hist_sd=sg*100,
               z_score=if(!is.na(sg)&&sg>1e-8)(cur-mu)/sg else NA_real_)
      })) %>%
        left_join(perf_data %>% select(symbol, short_name, asset_class),
                  by = c("ticker"="symbol")) %>%
        filter(!is.na(z_score)) %>% arrange(z_score)
    })

    output$surp_plot <- renderPlot({
      df <- surp_df()
      if (!nrow(df)) return(.empty_plot("No data"))
      win <- if (is.null(input$surp_win)) "1M" else input$surp_win
      df <- df %>% mutate(
        lbl      = .tk_label(ticker, short_name),
        lbl      = factor(lbl, levels = lbl),
        fill_col = dplyr::case_when(
          z_score >=  2 ~ "#1a5e35", z_score <= -2 ~ "#922b21",
          z_score >=  0 ~ "#27ae60", TRUE          ~ "#e74c3c"),
        txt_x = z_score + if_else(z_score >= 0, 0.12, -0.12)
      )
      xlim <- max(abs(df$z_score), 2.5, na.rm=TRUE) * 1.22
      nsig <- sum(abs(df$z_score) >= 2)
      bw   <- if (nrow(df) > 40) 0.55 else 0.70

      ggplot(df, aes(x=z_score, y=lbl)) +
        annotate("rect", xmin=-Inf, xmax=-2, ymin=-Inf, ymax=Inf, fill="#fde8e6", alpha=0.7) +
        annotate("rect", xmin= 2,   xmax=Inf,ymin=-Inf, ymax=Inf, fill="#e8f8f0", alpha=0.7) +
        geom_vline(xintercept=0, colour="#9ca3af", linewidth=0.4) +
        geom_vline(xintercept=c(-2,2), linetype="dashed", colour="#7f8c8d", linewidth=0.5) +
        geom_col(aes(fill=fill_col), width=bw) + scale_fill_identity() +
        geom_text(aes(x=txt_x, hjust=if_else(z_score>=0,0,1),
                      label=sprintf("%+.1f%% (z=%.2f)", cum_rel, z_score)),
                  colour="#2c3e50", size=2.7) +
        annotate("text",x=-2,y=nrow(df)+.6,label="\u22122\u03c3",colour="#7f8c8d",size=3,vjust=0)+
        annotate("text",x= 2,y=nrow(df)+.6,label="+2\u03c3", colour="#7f8c8d",size=3,vjust=0)+
        scale_x_continuous(limits=c(-xlim,xlim), expand=expansion(0)) +
        labs(title=sprintf("Surprise \u2014 %s  \u2502  %d/%d outside \u00b12\u03c3",win,nsig,nrow(df)),
             subtitle="Cumulative return vs SPY, z-scored against own rolling history",
             x="Z-score", y=NULL) +
        .pat_light_theme()
    }, bg="#f8f9fa")

    output$surp_tbl <- renderReactable({
      df <- surp_df(); if (!nrow(df)) return(NULL)
      df %>% arrange(desc(abs(z_score))) %>%
        mutate(label=.tk_label(ticker,short_name),
               flag=dplyr::case_when(
                 z_score>= 2~"\U1F7E2 Surge", z_score<=-2~"\U1F534 Drop",
                 z_score>= 1~"\u26AA Mild +", z_score<=-1~"\u26AA Mild \u2212",
                 TRUE~"\u2013 Neutral")) %>%
        select(label,asset_class,cum_rel,z_score,flag) %>%
        reactable(searchable=TRUE,striped=TRUE,highlight=TRUE,defaultPageSize=60,theme=tbl_theme,
          defaultColDef=colDef(vAlign="center",headerVAlign="bottom"),
          columns=list(
            label=colDef(name="Ticker",minWidth=150,
                         style=list(fontWeight="700",color=BLUE_TICK,fontSize="12px")),
            asset_class=colDef(name="Class",width=90,
                               style=list(color=TEXT_DIM,fontSize="11px")),
            cum_rel=colDef(name="Cum Rel%",width=85,format=colFormat(digits=1,suffix="%"),
                           style=function(v) list(
                             color=if(!is.na(v)&&v>=0)"#27ae60" else "#e74c3c",
                             fontWeight="700",fontSize="12px")),
            z_score=colDef(name="Z",width=65,format=colFormat(digits=2),
                           style=function(v) list(
                             color=if(!is.na(v)&&abs(v)>=2)"#fbbf24" else TEXT_DIM,
                             fontWeight=if(!is.na(v)&&abs(v)>=2)"700" else "400",
                             fontSize="12px")),
            flag=colDef(name="Signal",width=90,style=list(fontSize="11px"))))
    })

    # =========================================================================
    # 2. STATE FLIP
    # =========================================================================
    flip_df <- reactive({
      syms <- syms_r()
      syms <- syms[syms %in% unique(trend_signals_shiny$symbol)]
      if (!length(syms)) return(tibble())

      ts <- trend_signals_shiny %>% filter(symbol %in% syms) %>% arrange(symbol, date)

      bind_rows(lapply(syms, function(tk) {
        d <- ts %>% filter(symbol == tk)
        if (nrow(d) < 23L) return(NULL)
        cur <- slice_tail(d, n=1L)
        ago <- slice(d, max(1L, n() - 22L))
        tibble(
          ticker       = tk,
          sig_now      = cur$signal,
          sig_ago      = ago$signal,
          pct_above_ma = if (!is.na(cur$ma200) && cur$ma200 > 0)
                           (cur$adjusted / cur$ma200 - 1) * 100 else NA_real_,
          flipped      = sig_now != sig_ago
        )
      })) %>%
        left_join(perf_data %>% select(symbol, short_name, asset_class, ret_1m),
                  by = c("ticker"="symbol")) %>%
        mutate(
          direction = dplyr::case_when(
            flipped & sig_now == 1L ~ "\u2191 Crossed Above",
            flipped & sig_now == 0L ~ "\u2193 Crossed Below",
            sig_now == 1L           ~ "Holding Above",
            TRUE                    ~ "Holding Below")
        ) %>%
        arrange(desc(flipped), desc(sig_now), ticker)
    })

    output$flip_tbl <- renderReactable({
      df <- flip_df(); if (!nrow(df)) return(NULL)
      df %>%
        mutate(label = .tk_label(ticker, short_name),
               ret_1m_pct = ret_1m * 100) %>%
        select(label, asset_class, direction, pct_above_ma, ret_1m_pct) %>%
        reactable(searchable=TRUE,striped=TRUE,highlight=TRUE,defaultPageSize=60,theme=tbl_theme,
          defaultColDef=colDef(vAlign="center",headerVAlign="bottom"),
          columns=list(
            label=colDef(name="Ticker",minWidth=150,
                         style=list(fontWeight="700",color=BLUE_TICK,fontSize="12px")),
            asset_class=colDef(name="Class",width=90,
                               style=list(color=TEXT_DIM,fontSize="11px")),
            direction=colDef(name="200DMA State",width=130,
                             style=function(v) list(
                               color=dplyr::case_when(
                                 grepl("Crossed Above",v)~"#27ae60",
                                 grepl("Crossed Below",v)~"#e74c3c",
                                 grepl("Holding Above",v)~"#86efac",
                                 TRUE~"#fca5a5"),
                               fontWeight=if(grepl("Crossed",v))"700" else "400",
                               fontSize="12px")),
            pct_above_ma=colDef(name="% vs MA200",width=95,format=colFormat(digits=1,suffix="%"),
                                style=function(v) list(
                                  color=if(!is.na(v)&&v>=0)"#27ae60" else "#e74c3c",
                                  fontWeight="600",fontSize="12px")),
            ret_1m_pct=colDef(name="1M Ret%",width=80,format=colFormat(digits=1,suffix="%"),
                              style=function(v) list(
                                color=if(!is.na(v)&&v>=0)"#27ae60" else "#e74c3c",
                                fontSize="12px"))))
    })

    output$flip_plot <- renderPlot({
      df <- flip_df(); if (!nrow(df)) return(.empty_plot("No data"))
      df_p <- df %>%
        filter(!is.na(pct_above_ma)) %>%
        mutate(
          lbl      = .tk_label(ticker, short_name),
          fill_col = dplyr::case_when(
            flipped & sig_now==1L ~ "#1a5e35",
            flipped & sig_now==0L ~ "#922b21",
            sig_now==1L           ~ "#27ae60",
            TRUE                  ~ "#e74c3c")
        ) %>%
        arrange(pct_above_ma) %>%
        mutate(lbl = factor(lbl, levels = lbl))

      nflip <- sum(df_p$flipped, na.rm=TRUE)
      bw    <- if (nrow(df_p) > 40) 0.55 else 0.70

      ggplot(df_p, aes(x=pct_above_ma, y=lbl)) +
        geom_vline(xintercept=0, colour="#9ca3af", linewidth=0.5) +
        geom_col(aes(fill=fill_col), width=bw) + scale_fill_identity() +
        geom_text(aes(x=pct_above_ma+if_else(pct_above_ma>=0,.3,-.3),
                      hjust=if_else(pct_above_ma>=0,0,1),
                      label=sprintf("%+.1f%%",pct_above_ma)),
                  colour="#2c3e50", size=2.7) +
        geom_point(data=df_p %>% filter(flipped),
                   aes(x=0,y=lbl), shape=8, size=3, colour="#f59e0b") +
        scale_x_continuous(expand=expansion(mult=c(.12,.18))) +
        labs(title=sprintf("Distance to 200DMA  \u2502  %d flip%s this month",
                           nflip, if(nflip!=1)"s" else ""),
             subtitle="\u2605 = crossed 200DMA in last 22 days  \u2502  Deep = recent flip",
             x="% above / below 200DMA", y=NULL) +
        .pat_light_theme()
    }, bg="#f8f9fa")

    # =========================================================================
    # 3. ROTATION (RRG)
    # =========================================================================
    rrg_df <- reactive({
      syms  <- syms_r(); syms <- syms[syms %in% colnames(xts_rel_shiny) & syms != "SPY"]
      n_rs  <- as.integer(if (is.null(input$rrg_win)) 63L else input$rrg_win)
      n_mom <- 10L; n_trail <- 5L
      if (!length(syms)) return(tibble())

      mat <- as.matrix(xts_rel_shiny[, syms, drop=FALSE])
      nr  <- nrow(mat)
      if (nr < n_rs + n_mom + n_trail * 5L) return(tibble())

      tidx <- rev(seq(nr, by=-5L, length.out=n_trail))
      tidx <- pmax(tidx, n_rs + n_mom)

      bind_rows(lapply(syms, function(tk) {
        x <- mat[,tk]; x[is.na(x)] <- 0
        rs  <- vapply(n_rs:nr, function(i) sum(x[(i-n_rs+1L):i]), numeric(1L))
        rmu <- mean(rs,na.rm=TRUE); rsd <- sd(rs,na.rm=TRUE)
        rs_n <- if(rsd>1e-8) (rs-rmu)/rsd*10+100 else rep(100,length(rs))

        n2 <- length(rs_n)
        if (n2 <= n_mom) return(NULL)
        mom <- rs_n[(n_mom+1):n2] - rs_n[1:(n2-n_mom)]
        mmu <- mean(mom,na.rm=TRUE); msd <- sd(mom,na.rm=TRUE)
        mom_n <- if(msd>1e-8) (mom-mmu)/msd*10+100 else rep(100,length(mom))

        get_pt <- function(xi) {
          jr <- xi - n_rs + 1L; jm <- xi - n_rs - n_mom + 1L
          c(if(jr>=1&&jr<=length(rs_n)) rs_n[jr] else NA_real_,
            if(jm>=1&&jm<=length(mom_n)) mom_n[jm] else NA_real_)
        }
        pts <- lapply(tidx, get_pt)
        tibble(ticker=tk,
               rs_ratio = pts[[n_trail]][1], rs_mom = pts[[n_trail]][2],
               trail_rs  = list(sapply(pts,`[`,1)),
               trail_mom = list(sapply(pts,`[`,2)))
      })) %>%
        filter(!is.na(rs_ratio),!is.na(rs_mom)) %>%
        left_join(perf_data %>% select(symbol,short_name), by=c("ticker"="symbol")) %>%
        mutate(
          quadrant = dplyr::case_when(
            rs_ratio>=100 & rs_mom>=100 ~ "Leading",
            rs_ratio>=100 & rs_mom< 100 ~ "Weakening",
            rs_ratio< 100 & rs_mom< 100 ~ "Lagging",
            TRUE                        ~ "Improving"),
          q_col = dplyr::case_when(
            quadrant=="Leading"  ~ "#1a5e35", quadrant=="Weakening" ~ "#b45309",
            quadrant=="Lagging"  ~ "#922b21", TRUE                  ~ "#1e40af"))
    })

    output$rrg_plot <- renderPlot({
      df <- rrg_df()
      if (!nrow(df)) return(.empty_plot("Need tickers in xts_rel_shiny"))

      trail <- bind_rows(lapply(seq_len(nrow(df)), function(i)
        tibble(ticker=df$ticker[i], rs=df$trail_rs[[i]], mom=df$trail_mom[[i]],
               step=seq_along(df$trail_rs[[i]])))) %>%
        filter(!is.na(rs),!is.na(mom))

      ar <- c(df$rs_ratio, unlist(df$trail_rs)); am <- c(df$rs_mom, unlist(df$trail_mom))
      xl <- c(min(min(ar,na.rm=TRUE)-8, 88), max(max(ar,na.rm=TRUE)+8, 112))
      yl <- c(min(min(am,na.rm=TRUE)-8, 88), max(max(am,na.rm=TRUE)+8, 112))

      ggplot() +
        annotate("rect",xmin=100,xmax=Inf, ymin=100,ymax=Inf, fill="#e8f8f0",alpha=0.85)+
        annotate("rect",xmin=100,xmax=Inf, ymin=-Inf,ymax=100,fill="#fef9c3",alpha=0.85)+
        annotate("rect",xmin=-Inf,xmax=100,ymin=-Inf,ymax=100,fill="#fde8e6",alpha=0.85)+
        annotate("rect",xmin=-Inf,xmax=100,ymin=100,ymax=Inf, fill="#dbeafe",alpha=0.85)+
        annotate("text",x=xl[2]-1,y=yl[2]-1,label="Leading",  hjust=1,vjust=1,colour="#1a5e35",fontface="bold",size=5,alpha=0.7)+
        annotate("text",x=xl[2]-1,y=yl[1]+1,label="Weakening",hjust=1,vjust=0,colour="#b45309",fontface="bold",size=5,alpha=0.7)+
        annotate("text",x=xl[1]+1,y=yl[1]+1,label="Lagging",  hjust=0,vjust=0,colour="#922b21",fontface="bold",size=5,alpha=0.7)+
        annotate("text",x=xl[1]+1,y=yl[2]-1,label="Improving",hjust=0,vjust=1,colour="#1e40af",fontface="bold",size=5,alpha=0.7)+
        geom_vline(xintercept=100,colour="#9ca3af",linewidth=0.5)+
        geom_hline(yintercept=100,colour="#9ca3af",linewidth=0.5)+
        geom_path(data=trail,aes(x=rs,y=mom,group=ticker,alpha=step),
                  colour="#94a3b8",linewidth=0.8)+
        scale_alpha_continuous(range=c(0.1,0.5),guide="none")+
        geom_point(data=df,aes(x=rs_ratio,y=rs_mom,colour=q_col),size=6)+
        scale_colour_identity()+
        ggrepel::geom_text_repel(
          data=df,
          aes(x=rs_ratio,y=rs_mom,label=.tk_label(ticker,short_name),colour=q_col),
          size=3.2,fontface="bold",box.padding=0.4,point.padding=0.3,max.overlaps=30)+
        coord_cartesian(xlim=xl,ylim=yl)+
        labs(title="Relative Rotation Graph",
             subtitle="RS-Ratio > 100 = outperforming SPY  \u2502  RS-Momentum > 100 = accelerating  \u2502  Trail = last 4 weeks",
             x="RS-Ratio",y="RS-Momentum")+
        theme_minimal(base_size=11)+
        theme(plot.background=element_rect(fill="#f8f9fa",colour=NA),
              panel.background=element_rect(fill="#f8f9fa",colour=NA),
              panel.grid.major=element_line(colour="#e0e2e8",linewidth=0.4),
              panel.grid.minor=element_line(colour="#eef0f4",linewidth=0.2),
              axis.text=element_text(colour="#1B3A6B",face="bold",size=9),
              axis.title=element_text(colour="#1B3A6B",face="bold",size=9),
              plot.title=element_text(colour="#1c2833",face="bold",size=13),
              plot.subtitle=element_text(colour="#1B3A6B",size=10),
              plot.margin=margin(12,16,12,12))
    }, bg="#f8f9fa")

    # =========================================================================
    # 4. CORR SHIFT
    # =========================================================================
    corr_data <- reactive({
      syms <- syms_r(); syms <- syms[syms %in% colnames(xts_ret_shiny)]
      if (length(syms) < 3L) return(NULL)
      R   <- as.matrix(xts_ret_shiny[, syms])
      c1m <- cor(tail(R, 22L), use="pairwise.complete.obs")
      c3m <- cor(tail(R, 63L), use="pairwise.complete.obs")
      dlt <- c1m - c3m
      pairs <- tibble(tk1=rep(syms,each=length(syms)),
                      tk2=rep(syms,times=length(syms)),
                      cor_1m=as.vector(c1m), cor_3m=as.vector(c3m),
                      delta=as.vector(dlt)) %>%
        filter(tk1 < tk2) %>% arrange(desc(abs(delta)))
      list(delta=dlt, pairs=pairs, syms=syms)
    })

    output$corr_plot <- renderPlot({
      cd <- corr_data()
      if (is.null(cd)) return(.empty_plot("Need \u22653 tickers"))

      # Cap at 25 most-volatile tickers so tiles stay legible
      MAX_TICKERS <- 25L
      syms_full <- cd$syms
      syms <- if (length(syms_full) > MAX_TICKERS) {
        vars <- apply(as.matrix(tail(xts_ret_shiny[, syms_full], 63L)), 2, var, na.rm=TRUE)
        names(sort(vars, decreasing=TRUE))[seq_len(MAX_TICKERS)]
      } else syms_full

      R   <- as.matrix(xts_ret_shiny[, syms])
      c1m <- cor(tail(R, 22L), use="pairwise.complete.obs")
      c3m <- cor(tail(R, 63L), use="pairwise.complete.obs")
      dlt <- c1m - c3m

      long <- expand.grid(x=syms, y=syms, stringsAsFactors=FALSE) %>%
        as_tibble() %>%
        mutate(val = as.vector(dlt),
               x   = factor(x, levels=syms),
               y   = factor(y, levels=rev(syms))) %>%
        filter(x != y)

      n_shown  <- length(syms)
      txt_size <- if (n_shown <= 12) 2.8 else if (n_shown <= 18) 2.0 else NA
      subtitle_txt <- paste0(
        "1M \u2212 3M baseline  \u2502  Red = decoupled  \u2502  Blue = recoupled",
        if (length(syms_full) > MAX_TICKERS)
          sprintf("  \u2502  Showing top %d by volatility (of %d)", MAX_TICKERS, length(syms_full))
        else ""
      )

      p <- ggplot(long, aes(x=x, y=y, fill=val)) +
        geom_tile(colour="white", linewidth=0.4) +
        scale_fill_gradientn(
          colours = c("#922b21","#e74c3c","#fde8e6","#f5f5f5","#dbeafe","#3b82f6","#1e3a8a"),
          values  = scales::rescale(c(-.4,-.2,-.05,0,.05,.2,.4)),
          limits  = c(-.45,.45), oob=scales::squish,
          name    = "\u0394\u03c1\n(1M\u22123M)",
          guide   = guide_colorbar(barheight=12, barwidth=1.2)
        ) +
        labs(title    = "Correlation Structure Shift",
             subtitle = subtitle_txt,
             x=NULL, y=NULL) +
        theme_minimal(base_size=9) +
        theme(
          plot.background  = element_rect(fill="#f8f9fa", colour=NA),
          panel.background = element_rect(fill="#f8f9fa", colour=NA),
          panel.grid       = element_blank(),
          axis.text.x      = element_text(angle=45, hjust=1, colour="#1B3A6B",
                                          face="bold", size=max(7, 11-n_shown/3)),
          axis.text.y      = element_text(colour="#1B3A6B", face="bold",
                                          size=max(7, 11-n_shown/3)),
          plot.title       = element_text(colour="#1c2833", face="bold", size=12),
          plot.subtitle    = element_text(colour="#7f8c8d", size=9),
          legend.text      = element_text(colour="#2c3e50", size=8),
          legend.title     = element_text(colour="#2c3e50", size=8),
          plot.margin      = margin(8,8,8,8)
        )

      # Add cell values only when tiles are large enough
      if (!is.na(txt_size)) {
        p <- p + geom_text(aes(label=sprintf("%+.2f", val)),
                           size=txt_size,
                           colour=ifelse(abs(long$val) > 0.15, "white", "#2c3e50"))
      }
      p
    }, bg="#f8f9fa")

    output$corr_tbl <- renderReactable({
      cd <- corr_data(); if (is.null(cd)) return(NULL)
      cd$pairs %>% slice_head(n=30) %>%
        mutate(flag=dplyr::case_when(
          delta>= .3~"\U1F7E2 Recoupled", delta<=-.3~"\U1F534 Decoupled", TRUE~"\u2013")) %>%
        reactable(striped=TRUE,highlight=TRUE,defaultPageSize=30,theme=tbl_theme,
          defaultColDef=colDef(vAlign="center",headerVAlign="bottom"),
          columns=list(
            tk1=colDef(name="Ticker A",width=90,
                       style=list(fontWeight="700",color=BLUE_TICK,fontSize="12px")),
            tk2=colDef(name="Ticker B",width=90,
                       style=list(fontWeight="700",color=BLUE_TICK,fontSize="12px")),
            cor_1m=colDef(name="1M \u03c1",width=70,format=colFormat(digits=2),
                          style=list(fontSize="12px",color=TEXT_DIM)),
            cor_3m=colDef(name="3M \u03c1",width=70,format=colFormat(digits=2),
                          style=list(fontSize="12px",color=TEXT_DIM)),
            delta=colDef(name="\u0394\u03c1",width=70,format=colFormat(digits=2),
                         style=function(v) list(
                           color=if(!is.na(v)&&v>0)"#27ae60" else "#e74c3c",
                           fontWeight="700",fontSize="12px")),
            flag=colDef(name="Signal",width=100,style=list(fontSize="11px"))))
    })

    # =========================================================================
    # 5. MOMENTUM SCATTER
    # =========================================================================
    output$mom_plot <- renderPlot({
      df <- filtered() %>%
        filter(!is.na(ret_ytd), !is.na(ret_1m)) %>%
        mutate(
          ytd_pct = ret_ytd * 100,
          m1_pct  = ret_1m  * 100,
          quadrant = dplyr::case_when(
            ytd_pct >= 0 & m1_pct >= 0 ~ "Leading",
            ytd_pct >= 0 & m1_pct <  0 ~ "Weakening",
            ytd_pct <  0 & m1_pct >= 0 ~ "Improving",
            TRUE                        ~ "Lagging"),
          q_col = dplyr::case_when(
            quadrant == "Leading"   ~ "#1a5e35",
            quadrant == "Weakening" ~ "#b45309",
            quadrant == "Lagging"   ~ "#922b21",
            TRUE                    ~ "#1e40af")
        )

      if (!nrow(df)) return(.empty_plot("No data"))

      xl <- range(df$ytd_pct, na.rm=TRUE); xl <- xl + diff(xl) * c(-.08, .08)
      yl <- range(df$m1_pct,  na.rm=TRUE); yl <- yl + diff(yl) * c(-.08, .08)
      xl <- c(min(xl[1], -1), max(xl[2], 1))
      yl <- c(min(yl[1], -1), max(yl[2], 1))

      ggplot(df, aes(x=ytd_pct, y=m1_pct)) +
        annotate("rect", xmin=0,    xmax=Inf,  ymin=0,   ymax=Inf,  fill="#e8f8f0", alpha=0.85) +
        annotate("rect", xmin=0,    xmax=Inf,  ymin=-Inf,ymax=0,    fill="#fef9c3", alpha=0.85) +
        annotate("rect", xmin=-Inf, xmax=0,    ymin=-Inf,ymax=0,    fill="#fde8e6", alpha=0.85) +
        annotate("rect", xmin=-Inf, xmax=0,    ymin=0,   ymax=Inf,  fill="#dbeafe", alpha=0.85) +
        annotate("text", x=xl[2]-.3, y=yl[2]-.3, label="Leading",   hjust=1, vjust=1,
                 colour="#1a5e35", fontface="bold", size=5, alpha=0.7) +
        annotate("text", x=xl[2]-.3, y=yl[1]+.3, label="Weakening", hjust=1, vjust=0,
                 colour="#b45309", fontface="bold", size=5, alpha=0.7) +
        annotate("text", x=xl[1]+.3, y=yl[1]+.3, label="Lagging",   hjust=0, vjust=0,
                 colour="#922b21", fontface="bold", size=5, alpha=0.7) +
        annotate("text", x=xl[1]+.3, y=yl[2]-.3, label="Improving", hjust=0, vjust=1,
                 colour="#1e40af", fontface="bold", size=5, alpha=0.7) +
        geom_vline(xintercept=0, colour="#9ca3af", linewidth=0.5) +
        geom_hline(yintercept=0, colour="#9ca3af", linewidth=0.5) +
        geom_point(aes(colour=q_col), size=6) +
        scale_colour_identity() +
        ggrepel::geom_text_repel(
          aes(label=.tk_label(symbol, short_name), colour=q_col),
          size=3.2, fontface="bold",
          box.padding=0.4, point.padding=0.3, max.overlaps=30) +
        scale_x_continuous(labels=scales::label_percent(scale=1, suffix="%")) +
        scale_y_continuous(labels=scales::label_percent(scale=1, suffix="%")) +
        coord_cartesian(xlim=xl, ylim=yl) +
        labs(
          title    = sprintf("Momentum Scatter \u2014 %d tickers", nrow(df)),
          subtitle = "YTD performance vs 1M performance  \u2502  Quadrant = regime position",
          x        = "YTD Return",
          y        = "1M Return"
        ) +
        theme_minimal(base_size=11) +
        theme(
          plot.background  = element_rect(fill="#f8f9fa", colour=NA),
          panel.background = element_rect(fill="#f8f9fa", colour=NA),
          panel.grid.major = element_line(colour="#e0e2e8", linewidth=0.4),
          panel.grid.minor = element_line(colour="#eef0f4", linewidth=0.2),
          axis.text        = element_text(colour="#1B3A6B", face="bold", size=9),
          axis.title       = element_text(colour="#1B3A6B", face="bold", size=9),
          plot.title       = element_text(colour="#1c2833", face="bold", size=13),
          plot.subtitle    = element_text(colour="#1B3A6B", size=10),
          plot.margin      = margin(12,16,12,12))
    }, bg="#f8f9fa")

    # =========================================================================
    # 7. ANALOGUE
    # =========================================================================
    ana_df <- reactive({
      syms <- syms_r(); syms <- syms[syms %in% colnames(xts_ret_shiny)]
      if (length(syms) < 3L) return(tibble())
      R  <- as.matrix(xts_ret_shiny[, syms]); nr <- nrow(R); nd <- 22L
      if (nr < nd * 5L) return(tibble())

      cv   <- colSums(tail(R, nd), na.rm=TRUE)
      cnrm <- cv / sqrt(sum(cv^2, na.rm=TRUE))
      dts  <- zoo::index(xts_ret_shiny)
      steps <- seq(nd, nr - nd, by=5L)

      bind_rows(lapply(steps, function(ei) {
        hv   <- colSums(R[(ei-nd+1L):ei,], na.rm=TRUE)
        hnrm <- hv / sqrt(sum(hv^2, na.rm=TRUE))
        cs   <- sum(cnrm * hnrm, na.rm=TRUE)
        fe   <- min(ei + nd,  nr); f3 <- min(ei + 63L, nr)
        tibble(
          date    = dts[ei],
          cos_sim = cs,
          fwd_1m  = if("SPY"%in%syms&&ei<nr) sum(R[(ei+1L):fe, "SPY"],na.rm=TRUE)*100 else NA_real_,
          fwd_3m  = if("SPY"%in%syms&&ei<nr) sum(R[(ei+1L):f3, "SPY"],na.rm=TRUE)*100 else NA_real_
        )
      })) %>%
        filter(!is.na(cos_sim), cos_sim < 0.9999) %>%
        arrange(desc(cos_sim)) %>% slice_head(n=10L)
    })

    output$ana_tbl <- renderReactable({
      df <- ana_df(); if (!nrow(df)) return(NULL)
      df %>% mutate(rank=row_number()) %>%
        reactable(striped=TRUE,highlight=TRUE,defaultPageSize=10,theme=tbl_theme,
          defaultColDef=colDef(vAlign="center",headerVAlign="bottom"),
          columns=list(
            rank=colDef(name="#",width=40,
                        style=list(fontWeight="700",color="#f59e0b",fontSize="13px")),
            date=colDef(name="Analogue Date",width=120,
                        format=colFormat(date=TRUE,locales="en-US"),
                        style=list(fontWeight="700",color=BLUE_TICK,fontSize="12px")),
            cos_sim=colDef(name="Similarity",width=90,format=colFormat(digits=3),
                           style=function(v) list(
                             color=if(!is.na(v)&&v>0.8)"#fbbf24" else TEXT_DIM,
                             fontWeight="700",fontSize="12px")),
            fwd_1m=colDef(name="SPY +1M",width=85,format=colFormat(digits=1,suffix="%"),
                          style=function(v) list(
                            color=if(!is.na(v)&&v>=0)"#27ae60" else "#e74c3c",
                            fontWeight="700",fontSize="12px")),
            fwd_3m=colDef(name="SPY +3M",width=85,format=colFormat(digits=1,suffix="%"),
                          style=function(v) list(
                            color=if(!is.na(v)&&v>=0)"#27ae60" else "#e74c3c",
                            fontWeight="700",fontSize="12px"))))
    })

    output$ana_plot <- renderPlot({
      df <- ana_df(); if (!nrow(df)) return(.empty_plot("Insufficient history"))
      df_p <- df %>%
        mutate(rank=factor(row_number(), levels=rev(row_number())),
               lbl=format(date,"%b %Y"))

      lbl_map <- setNames(df_p$lbl, as.character(df_p$rank))

      p1 <- ggplot(df_p, aes(x=cos_sim, y=rank)) +
        geom_col(fill="#3b82f6", alpha=0.55, width=0.65) +
        geom_text(aes(x=cos_sim, label=sprintf(" %.3f", cos_sim)),
                  hjust=0, colour="#1c2833", size=3.2, fontface="bold") +
        scale_x_continuous(expand=expansion(mult=c(0, 0.18))) +
        scale_y_discrete(labels=lbl_map) +
        labs(title="Nearest Analogues", x="Cosine similarity", y=NULL) +
        theme_minimal(base_size=10) +
        theme(plot.background=element_rect(fill="#f8f9fa",colour=NA),
              panel.background=element_rect(fill="#f8f9fa",colour=NA),
              panel.grid=element_blank(),
              axis.text.y=element_text(colour="#1B3A6B",face="bold",size=9),
              axis.text.x=element_text(colour="#1B3A6B",face="bold",size=8),
              axis.title.x=element_text(colour="#1B3A6B",face="bold",size=9),
              plot.title=element_text(colour="#1c2833",face="bold",size=11),
              plot.margin=margin(8,4,8,8))

      long_fwd <- df_p %>%
        tidyr::pivot_longer(c(fwd_1m,fwd_3m), names_to="horizon", values_to="ret")

      p2 <- ggplot(long_fwd, aes(x=ret, y=rank, fill=horizon)) +
        geom_col(position="dodge", width=0.6, alpha=0.85) +
        scale_fill_manual(values=c(fwd_1m="#3b82f6",fwd_3m="#8b5cf6"),
                          labels=c(fwd_1m="+1M SPY",fwd_3m="+3M SPY")) +
        geom_vline(xintercept=0, colour="#9ca3af", linewidth=0.4) +
        labs(title="What Happened Next (SPY)", x="Forward return %", y=NULL, fill=NULL) +
        theme_minimal(base_size=10) +
        theme(plot.background=element_rect(fill="#f8f9fa",colour=NA),
              panel.background=element_rect(fill="#f8f9fa",colour=NA),
              panel.grid.major.y=element_blank(),
              panel.grid.major.x=element_line(colour="#e8eaed",linewidth=0.3),
              panel.grid.minor=element_blank(),
              axis.text.y=element_blank(),
              axis.text.x=element_text(colour="#1B3A6B",face="bold",size=8),
              axis.title.x=element_text(colour="#1B3A6B",face="bold",size=9),
              plot.title=element_text(colour="#1c2833",face="bold",size=11),
              legend.position="bottom",
              legend.text=element_text(colour="#2c3e50",size=9),
              plot.margin=margin(8,8,8,4))

      patchwork::wrap_plots(p1, p2, ncol=2, widths=c(1,1.2))
    }, bg="#f8f9fa")

  })
}
