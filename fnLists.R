fnLists <- function() {
        
        ##--------- Items with corrupt inaccurate data
        ## ---- C-ZARCAD, C-BTCZAR, JSE-GOLD
        # All lists used in data
       
            
        Indexes <- unique(c( "JSE-ALSH", "JSE-LGCAP", "JSE-RESI", "JSE-INDI", "JSE-FINI", "JSE-TECH", "JSE-TELE", "JSE-HEAL", "JSE-REALE", "JSE-CONSD", "JSE-CONST", "JSE-ENRGY", "JSE-BANK", "JSE-LIFE", "JSE-CONSS", "JSE-RETAL", "JSE-BEVR", "JSE-FOOD", "JSE-TABAC", "JSE-PDG", "JSE-CONM", "JSE-INDM", "JSE-CHES", "JH-ALSI40", "SATRIX500", "ETFPLT", "NIKKEI", "C-USDZAR", "GOLD-R", "SILV-R","C-BTCZAR","C-ETHZAR","JSE-GOLD","JSE-PLAT"))
        IND <- c("JSE-FINI", "GOLD-R", "SILV-R", "JSE-INDI", "C-USDZAR", "JSE-TECH", "NIKKEI", "JSE-PLAT", "JSE-METL", "UALSB", "JSE-INDM", "JSE-FOOD", "JSE-PHAR", "JSE-OILG", "JSE-HEAL", "SP500", "JH-ALSI40", "JSE-COAL", "JSE-RESI", "JSE-ALTX", "JSE-BANK", "BRENT", "JSE-AUTM", "JSE-CHES")
        PDG <- c("BIDCORP", "CLICKS", "DIS-CHEM", "PICKNPAY", "SHOPRIT", "SPAR")
        CONM <- c("PPC", "RAUBEX", "WBHO")
        LIFE <- c("DISCOVERY", "MOMENTUM", "SANLAM", "OMUTUAL")
        CONSS <- c("ADVTECH", "CURRO")
        CHES <- c("AECI", "OMNIA", "SASOL")
        PLAT <- c("RBPLAT", "IMPLATS", "NORTHAMH", "SIBANYE-S", "THARISA","VALTERRA")
        GOLD <- c("ANGGOLD", "PAN-AF", "DRDGOLD", "HARMONY", "GFIELDS")
        INDM <- c("KUMBAIO")
        BANK <- c("ABSAG", "STANBANK", "NEDCOR", "FIRSTRAND", "CAPITEC", "DISCOVERY")
        FOOD <- c("RCL", "CLOVER", "RHODES", "A-V-I", "OCEANA", "ASTRAL", "TONGAAT", "PNR-FOODS", "TIGBRANDS", "JSE-FOOD")
        METL <- c("GLENCORE", "ARM", "ANGLO", "BHPGROUP")
        CONS <- c("ADVTECH", "BIDCORP", "CITYLDG", "CLICKS", "CURRO", "CASHBIL", "FAMBRANDS", "ITLTILE", "MRPRICE", "MASSMART", "NASPERSN", "PICKNPAY", "SHOPRIT", "SPAR", "SUNINT", "TFG", "TRUWTHS", "WOOLIES")
        TECH <- c("PROSUS", "DATATEC", "ALTRON", "NASPERSN", "IOCOLIM")
        RESI <- c("ANGLO", "ANGGOLD", "GLENCORE", "GFIELDS", "HARMONY", "IMPLATS", "SIBANYE-S", "SASOL", "BHPGROUP")
        HEAL <- c("ASPEN", "ADCOCK", "LIFEHC", "NETCARE")
        TELE <- c("BLUETEL", "TELKOM", "MTN", "VODACOM")
        PHAR <- c("ASCENDIS", "ASPEN", "ADCOCK")
        OILG <- c("MNTKRENEW", "SASOL")
        ENRGY <- c("MNTKRENEW", "EXXARO", "TGALTD")
        BEVR <- c("ABINBEV")
        COAL <- "EXXARO"
        NEWGOLD <- "NEWGOLD"
        INDI <- c("ABINBEV", "ASPEN", "AVLN", "BIDVEST", "RICHEMONT", "CLICKS", "LIFEHC", "MTN", "NASPERSN", "NETCARE", "PICKNPAY", "PROSUS", "SHOPRIT", "SPAR", "TIGBRANDS", "TFG", "TRUWTHS", "VODACOM", "WOOLIES", "MCGROUP", "MRPRICE", "BATS", "BARWORLD", "BIDCORP")
        FINI <- c("CAPITEC", "DISCOVERY", "FIRSTRAND", "INVPLC", "NEDCOR", "NEPIROCK", "OMUTUAL", "REDEFINE", "REMGRO", "REINET", "SANLAM", "STANBANK", "ABSAG")
        
        CURR <- c("C-AUDUSD", "C-AUDZAR", "C-CADEUR", "C-CADGBP", "C-CADUSD", "C-CADZAR", "C-CHFUSD", "C-CHFZAR", "C-EURCAD", "C-EURGBP", "C-EURJPY", "C-EURUSD", "C-EURZAR", "C-GBPCAD", "C-GBPUSD", "C-GBPZAR", "C-JPYAUD", "C-JPYEUR", "C-JPYUSD", "C-NZDUSD", "C-NZDZAR", "C-USDAUD", "C-USDCAD", "C-USDEUR", "C-USDGBP", "C-USDJPY", "C-USDNZD", "C-USDRUB", "C-USDZAR", "C-ZARUSD", "C-USDINR", "C-USDCNY", "C-ZARNZD", "C-ZARAUD", "C-ZAREUR", "C-YENZAR", "C-ZARGBP", "C-SWFZAR", "GOLD-R", "SILV-R", "C-ETHZAR","C-BTCZAR", "C-ZARCHF")
        
        Commods <- c("COBALTM", "NICKEL", "ZINC-$", "GOLD-NY", "WTI_CRUDE", "RHODIUM", "SILV-$", "GOLD-PM", "PALLAD-$", "BRENT", "COPPER-$", "URANIUM", "PLAT-R", "GOLD-R", "SILV-R", "RJCRB-TRI")
        RETAIL <- c("CASHBUILD", "ITALTILE", "MRPRICE", "MOTUS", "PEPKOR", "TFG", "TRUWTHS", "WOOLIES")
        zar <- c("C-ZARBWP", "C-ZARNZD", "C-ZARUSD", "C-SWFZAR", "C-YENZAR", "C-ZAREUR", "C-ZARGBP", "C-ZARAUD", "C-EURZAR", "C-USDZAR", "C-AUDZAR", "C-CADZAR", "C-GBPZAR", "C-NZDZAR", "SILV-R", "GOLD-R", "PLAT-R", "C-ETHZAR","C-BTCZAR")
        
        excl_list <- c("AWAPROPB","AGLSBB", "AMSSBE", "AMSSBP", "ANGSBA", "ANGSBO", "ANGSBY", "APNSBG", "APNSBH", "ASHINF", "ASHMID", "ASHT40", "ASHWGB", "BLUSBH", "BHPSBF", "CSP500", "CSPROP", "CTOP50", "DIPULA-A", "DIPULA-B", "EQUITES", "ETF5IT", "ETFGLD", "ETFPLD", "ETFPLT", "ETFSAP", "FIRSTRNDP", "GFISBC", "GFISBD", "GFISBT", "HARSBW", "HARSBO", "IMPSBH", "IMPSBI", "IMPSBV", "INDLU", "J200USD", "JH-ALEX", "JH-ALSI40", "JH-ASIN", "JH-FLED", "JH-MIDCAP", "JH-RES", "JH-SMALL", "JSE", "JSE-ALPI", "JSE-ALSH", "JSE-ALT15", "JSE-ALTX", "JSE-AUTM", "JSE-BANK", "JSE-BASM", "JSE-BEVR", "JSE-CALS", "JSE-CHES", "JSE-CIN25", "JSE-COAL", "JSE-CONG", "JSE-CONM", "JSE-CONS", "JSE-CPI", "JSE-CTOP", "JSE-DALS", "JSE-DIVP", "JSE-DTOP", "JSE-ELEE", "JSE-EQII", "JSE-FINA", "JSE-FINDI", "JSE-FINI", "JSE-FJGI", "JSE-FJVI", "JSE-FOOD", "JSE-FOOR", "JSE-FORE", "JSE-FTEL", "JSE-GENF", "JSE-GENI", "JSE-GERE", "JSE-HCOM", "JSE-HEAL", "JSE-HEES", "JSE-HOUS", "JSE-IIND", "JSE-INDE", "JSE-INDI", "JSE-INDM", "JSE-INDT", "JSE-LGCAP", "JSE-LIFE", "JSE-MEDI", "JSE-METL", "JSE-MINI", "JSE-MTEL", "JSE-NLIF", "JSE-OILG", "JSE-OILP", "JSE-PERG", "JSE-PHAR", "JSE-PLAT", "JSE-PRUT", "JSE-PULS", "JSE-REDS", "JSE-REIV", "JSE-RESI", "JSE-SAPI", "JSE-SCOM", "JSE-SCTP", "JSE-SRI", "JSE-SUPS", "JSE-SW40", "JSE-SWALS", "JSE-SXTRI", "JSE-TABA", "JSE-TECH", "JSE-TELE", "KIOSBN", "MRPSBG", "NPNSBA", "MTNSBB", "GFISBU", "SBKSBB", "NPNSBB", "NPNSBC", "NPNSBP", "NPNSBZ", "Nedbank-P", "PRXSBQ", "PSG-KST", "SATRIX40", "SATRIX500", "SATRIXDIV", "SATRIXEMG", "SATRIXNDQ", "SATRIXRAF", "SATRIXRES", "SATRIXSWX", "SATRIXWRD", "SBEN05", "SBKSBO", "SGLSBC", "SGLSBD", "SGLSBE", "SGLSBU", "SGLSTD", "SHPSBG", "SOLSBC", "SOLSBE", "STANBANKP", "STXFIN", "STXILB", "STXIND", "STXPRO", "STXQUA", "SYG4IRGE", "SYGEURO50", "SYGSP500", "SYGSWIX40", "TOPSBA", "TOPSBB", "TOPSBN", "TOPSBO", "TOPSBW", "TOPSBX", "TOPSBY", "UBNPNE", "VODSBG", "ZA077", "ZA079", "WHLSBD", "WHLSBE", "ZAMBEZI-P","PRXSBB","SMPSBA","SOLSBA","JSE-INVBS","WNXT40","AGLSBF","PCWAMETF","WHLSBA","MTNZF","BAFAMETF","EGEAMETF","WTOP20","MTNSBQ","CSYSB","WTOP20","HARSBF","SSWSBI")
        
        # Return all lists as a named list
        return(list(
                excl_list = excl_list,
                Indexes = Indexes,
                IND = IND,
                PDG = PDG,
                CONM = CONM,
                LIFE = LIFE,
                CONSS = CONSS,
                CHES = CHES,
                PLAT = PLAT,
                GOLD = GOLD,
                INDM = INDM,
                BANK = BANK,
                FOOD = FOOD,
                METL = METL,
                CONS = CONS,
                TECH = TECH,
                RESI = RESI,
                HEAL = HEAL,
                TELE = TELE,
                PHAR = PHAR,
                OILG = OILG,
                ENRGY = ENRGY,
                BEVR = BEVR,
                COAL = COAL,
                NEWGOLD = NEWGOLD,
                INDI = INDI,
                FINI = FINI,
                CURR = CURR,
                Commods = Commods,
                RETAIL = RETAIL,
                zar = zar
        ))
}
