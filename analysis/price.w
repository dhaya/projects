rtresults.csv <- rtresults.xls
    soffice --headless --convert-to csv rtresults.xls

pfresults.csv <- pfresults.xls
    soffice --headless --convert-to csv pfresults.xls

out.csv <- pfresults.csv, rtresults.csv [shell]
    sqlite3 -init sqlite.init pricing.db > $OUTPUT
