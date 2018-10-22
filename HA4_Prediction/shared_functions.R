# Feedo shared functions

library(RODBC)
library(magrittr)
library(dplyr)
library(tibble)
library(config)

Sys.setenv("plotly_username" = "ftrojan")
Sys.setenv("plotly_api_key" = "xwy1wkd9vx")
options(dplyr.width = Inf) # print all columns in dplyr print table as per http://stackoverflow.com/questions/22471256/overriding-variables-not-shown-in-dplyr-to-display-all-columns-from-df
config = config::get()
# Database connection

test_db = function() {
  dbname = config$db
  tmp = tryCatch({
    SQLcon = odbcDriverConnect(sprintf("driver=SQL Server; server=10.94.253.9;database=%s", dbname))
    cat(sprintf("Connection to %s succeeded\n", dbname))
    return(SQLcon)
  }, error = function(e){
    logger(sprintf('DB connection: %s', e), 'ERROR')
    cat(sprintf("Connection to %s failed with error: %s\n", dbname, e))
    return(NULL)
  })
}

sql_db = function(query) {
  dbname = config$db
  tmp = tryCatch({
    SQLcon = odbcDriverConnect(sprintf("driver=SQL Server; server=10.94.253.9;database=%s", dbname))
    table = sqlQuery(SQLcon, query, stringsAsFactors = FALSE, errors = TRUE)
    close(SQLcon)
    return(as_tibble(table))
  }, error = function(e){
    logger(sprintf('DB connection: %s', e), 'ERROR')
    cat(sprintf("Connection to %s failed with error: %s\n", dbname, e))
    return(NULL)
  })
}

save_db = function(df, table_name, append = FALSE, ...) {
  dbname = config$db
  tmp = tryCatch({
    SQLcon = odbcDriverConnect(sprintf("driver=SQL Server; server=10.94.253.9;database=%s", dbname))
    if (!append) sqlQuery(SQLcon, sprintf("if object_id('%s') is not null drop table %s", table_name, table_name), errors = TRUE)
    # special treatment of date/datetime - see also http://stackoverflow.com/questions/8526278/r-rodbc-sqlsave-mapping-dataframe-timestamps-to-sql-server-timestamps
    dft = sapply(df, class)
    ddt = grep("POSIX", dft)
    ddd = grep("Date", dft)
    dd = c(ddt, ddd)
    if (length(dd)>0) { # there are date/datetime fields
      vtdt = rep("datetime", length(ddt))
      names(vtdt) = names(df)[ddt]
      vtdd = rep("date", length(ddd))
      names(vtdd) = names(df)[ddd]
      vt = c(vtdt,vtdd)
      res = sqlSave(SQLcon, df, tablename = table_name, rownames = FALSE, varTypes = vt, append = append, ...)
    } else { # no date/datetime fields
      res = sqlSave(SQLcon, df, tablename = table_name, rownames = FALSE, append = append, ...)
    }
    close(SQLcon)
    return(res)
  }, error = function(e){
    logger(sprintf('DB connection: %s', e), 'ERROR')
    cat(sprintf("Connection to %s failed with error: %s\n", dbname, e))
    return(NULL)
  })
}

run_s = function(script, logdir = sprintf('%s/logs/', config$outdata), ...) {
  #logger(sprintf("%s start", script), logdir = ldir)
  cmd = sprintf("call sqlcmd -S CZPRG0715 -d %s -i %s", config$db, script)
  cat(sprintf("%s %s\n", Sys.time(), cmd))
  shell(cmd, translate = TRUE, ...)
  #logger(sprintf("%s end", script), logdir = ldir)
}

run_ss = function(script, ...) {
  logger(sprintf("%s start", script))
  cmd = sprintf("call sqlcmd -S CZPRG0715 -d %s -i %s", config$db, script)
  cat(sprintf("%s %s\n", Sys.time(), cmd))
  shell(cmd, translate = TRUE, ...)
  logger(sprintf("%s end", script))
}

run_rr = function(script, ...) {
  logger(sprintf("%s start", script))
  cat(sprintf("%s %s\n", Sys.time(), script))
  source(script, ...)
  logger(sprintf("%s end", script))
}

# Logging

#------------------------------------------------------------
# ORDINARY LOG FILE ENTRY
#------------------------------------------------------------
#------------------------------------------------------------
logger = function(logmsg, type='INFO', LOG.fn='Rscript', logdir = sprintf('%s/logs/', config$outdata)) {
  # Log file entry
  logfile = paste0(logdir, format(Sys.Date(), '%Y-%m-%d'), '.log')
  logfn = as.character(sys.call(-1)[[1]])
  if(!dir.exists(logdir)){
    dir.create(logdir)
  }
  if (!file.exists(logfile)) {
    f = file(logfile, "w")
    close(f)
  }
  try(cat(sprintf('%-20s %-10s', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), type)
    , sprintf('%-16s', Sys.getenv("USERNAME"))
    , sprintf('Function: %-32s', logfn)
    , sprintf('%-32s', logmsg)
    , '\n'
    , sep = '|'
    , file    = logfile
    , append  = T
  ), silent = T)
}

# PDF reporting

star_significance = function(pvalue) {
  if (is.na(pvalue)) return("NA")
  else if (pvalue <= 0.001) return("***")
  else if (pvalue <= 0.01) return("**")
  else if (pvalue <= 0.05) return("*")
  else if (pvalue <= 0.1) return(".")
  else if (pvalue <= 1) return("!!!")
}

cfmt = function(x) {
  # currency formatting
  return(formatC(x, format="f", digits=0, big.mark = ","))
}

# dplyr enhancements

cross_join = function(a, b) {
  # cross join of two data frames
  ak = a %>% mutate(cross_key = 1)
  bk = b %>% mutate(cross_key = 1)
  return(ak %>% inner_join(bk, by = "cross_key") %>% select(-cross_key))
}

mutate_cond <- function(.data, condition, ..., envir = parent.frame(), nawarn = TRUE) {
  # mutate_cond Create a simple function for data frames or data tables that can be incorporated into pipelines. 
  # This function is like mutate but only acts on the rows satisfying the condition
  # example: DF %>% mutate_cond(measure == 'exit', qty_exit = qty, cf = 0, delta_watts = 13)
  condition <- eval(substitute(condition), .data, envir)
  ina = which(is.na(condition))
  if (length(ina)>0) {
    if (nawarn) {warning(sprintf("NA result occured %d times in condition evaluation in mutate_cond, treated as FALSE", length(ina)))}
    condition[ina] = FALSE
  }
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

check_unique = function(x, col = 1) {
  if (any(c("data.frame", "tibble") %in% class(x))) {
    df = x
  } else {
    df = tibble(x = x)
  }
  colname = names(x)[col]
  ux = df %>% group_by_(colname) %>% summarize(n_dup = n())
  if (nrow(df)==nrow(ux)) {
    cat("Vector is unique.\n")
  } else {
    dv = ux %>% filter(n_dup>1)
    dupl = df %>% inner_join(dv, by = colname) %>% arrange_(quote(desc(n_dup)), colname)
    cat(sprintf("Vector is not unique. It has %d duplicated values and %d repetitions. Top 10 are listed below.", nrow(dv), nrow(dupl)))
    print(head(dupl, 10))
  }
}

# date utilities

dow = function(d, k) {
  # returns specified day of that week (k=1 is Monday, k=7 is Sunday)
  dc = as.POSIXct(d)
  dl = as.POSIXlt(d)
  wd1 = dl$wday # 0–6 day of the week, starting on Sunday.
  wd2 = ifelse(wd1 - 1 >= 0, wd1 - 1, wd1 + 6) # 0–6 day of the week, starting on Monday.
  y = as.Date(d) + as.difftime(-wd2+k-1, units = "days")
  return(y)
}

library(lubridate)

difft = function(t2, t1, unit) {
  if (unit=="months") {
    t_dur = interval(t1, t2) %/% months(1)
  } else {
    t_dur = difftime(t2, t1, units = unit)
  }
  return(t_dur)
}

timebox = function(
  unit, # month, days, weeks
  DSD, # date
  DED, # date
  target_period, # difftime
  blackout_period, # difftime
  maximum_depth, # difftime
  make_plot = FALSE
) {
  dsd = as.Date(DSD)
  ded = as.Date(DED)
  tot_dur = difft(ded, dsd, unit)
  ow_dur = tot_dur - target_period - maximum_depth
  if (unit=="months") {
    md = months(maximum_depth)
    tp = months(target_period)
    bp = months(blackout_period)
    ow_start = as.Date(DSD) %m+% md
    ow_end = as.Date(DED) %m+% months(-target_period)
    los = dsd %m+% months(tot_dur - target_period - maximum_depth)
    fts = ow_start %m+% bp
    lts = ow_end %m+% bp
    fte = ow_start %m+% tp
  } else {
    md = as.difftime(maximum_depth, units = unit)
    tp = as.difftime(target_period, units = unit)
    bp = as.difftime(blackout_period, units = unit)
    ow_start = as.Date(DSD) + md
    ow_end = as.Date(DED) - tp
    los = dsd + ow_dur # last observation start
    fts = ow_start+bp # first target start
    lts = fts+ow_dur # last target start
    fte = ow_start+tp # first target end
  }
  if (make_plot) {
    Gray50 = rgb(0.5,0.5,0.5)
    plot.new()
    plot.window(xlim=c(dsd,ded), ylim=c(ow_start,ow_end))
    # bottom axis
    xa = c(dsd, ow_start, ow_end, ded)
    xl = sprintf("%s", xa)
    ba = axis(side=1, at = xa, labels = xl, tick = TRUE, line = NA,
              pos = NA, outer = FALSE, font = NA, lty = "solid",
              lwd = 1, lwd.ticks = 1, col = Gray50, col.ticks = Gray50,
              hadj = NA, padj = NA)
    # left axis
    ya = c(ow_start, ow_end)
    yl = sprintf("%s", ya)
    la = axis(side=2, at = ya, labels = yl, tick = TRUE, line = NA,
              pos = NA, outer = FALSE, font = NA, lty = "solid",
              lwd = 1, lwd.ticks = 1, col = Gray50, col.ticks = Gray50,
              hadj = NA, padj = NA)
    # time box
    rect(xleft=dsd, ybottom=ow_start, xright=ded, ytop=ow_end,
         col = "white", border = "black", lty = "solid", lwd = 1)
    # predictors trapezoid
    polygon(x = c(dsd, los, ow_end, ow_start, dsd),
            y = c(ow_start, ow_end, ow_end, ow_start, ow_start),
            col = "blue", border = "black", lty = "solid", lwd = 1)
    # target trapezoid
    polygon(x = c(fts, lts, ded, fte, fts),
            y = c(ow_start, ow_end, ow_end, ow_start, ow_start),
            col = "red", border = "black", lty = "solid", lwd = 1)
    # text(xa, ybaradj, barlabels, cex=1, pos=3, col=color_ybar)
    # lines(x=xa, y=ylineadj, col = color_yline, type="l", lwd=3)
    # lines(x=xa, y=ylineadj, col = color_yline, type="p", lwd=1, pch=15, cex=1.5)
    # text(xa, ylineadj, linelabels, cex=1, pos=3, col=color_yline)
    title(main="Model time box", xlab=sprintf("time (%.0f %s)", tot_dur, unit), ylab = sprintf("observation window (%.0f %s)", ow_dur, unit))
    
  }
  res = list(
    DSD = dsd,
    DED = ded,
    total_duration = tot_dur,
    target_period = tp,
    blackout_period = bp,
    maximum_depth = md,
    ow_start = ow_start,
    ow_end = ow_end,
    ow_duration = ow_dur
  )
  return(res)
}

