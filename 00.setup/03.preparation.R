


# : ===========================================================

### Data Summary Function ================================ 



summary_upload <- function(DT) {
    
    
is.date <- function(x) inherits(x, 'Date')


### Basic Strcture
name = deparse(substitute(DT))
nrows = prettyNum(nrow(DT), digits = 0, format = 'f', big.mark = '.', decimal.mark = ',') 
ncols = prettyNum(ncol(DT), digits = 0, format = 'f', big.mark = '.', decimal.mark = ',') 


### Column Types
summed_names = names(DT)[-1]

cols.character = sum(DT[, lapply(.SD, is.character), .SDcols = summed_names])
cols.numeric = sum(DT[, lapply(.SD, is.numeric), .SDcols = summed_names])
cols.factor = sum(DT[, lapply(.SD, is.factor), .SDcols = summed_names])
cols.date = sum(DT[, lapply(.SD, is.date), .SDcols = summed_names])




### TABLE FOR EXPORT

names_col = c('Object Name', 'Number of Rows', 'Number of Columns', 'Date Columns', 'Numeric Columns', 'Factor Columns', 'Other Columns')
value_col = c(name, nrows, ncols, cols.date, cols.numeric, cols.factor, cols.character)

dt_sum = data.frame(Chracteristic = names_col,
                    Value = value_col) 





}




### Graph Margins setup ----------------------------------------------------------

m_1 <- list(
            l = 30,
            r = 30,
            b = 50,
            t = 30
          )
