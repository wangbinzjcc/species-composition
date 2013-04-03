#########################################################
# 计算 物种IV（重要值）   王斌  2012年4月4日15:20:11
#########################################################

  IV.Calc <- function( datas ,side = 20 ,plotdim=c(500,300)){
        if(any(datas$x==0 | datas$y==0)){
            datas[datas$x==0,]$x <- 0.05 ; datas[datas$x==0,]$y <- 0.05
                                        }
        datas$sp <- as.character(datas$sp)                                 
        #相对多度：
        sp.abu <- table(datas$sp)
        rel.den <- sp.abu*100/sum(sp.abu)

        #相对优势度：
        sp.dbh <- tapply(datas$dbh ,datas$sp ,function(x){sum(pi*x^2/4)})
        rel.dom <- sp.dbh*100/sum(sp.dbh)

        #相对频度：
        sp.fre <-tapply( paste(ceiling(datas$x/side) ,ceiling(datas$y/side))
                        ,datas$sp ,function(x)sum(table(x)>0) )
        rel.fre <- sp.fre*100/sum(sp.fre)

        #结果：
        results <- data.frame(rel.den=as.numeric(rel.den) ,rel.dom=rel.dom ,rel.fre=rel.fre  
                              ,IV=as.numeric(rel.den+rel.dom+rel.fre)/3)
        results <- results[order(-results$IV),]
        
        #输出:
        return(round(results,3))
                                                         }

##################################

# 原始数据  表头包括  "sp", "x", "y", "dbh"
#   datas <- read.csv("ngdata83.1.csv")

# 统计程序运行时间   
#   time00 <- Sys.time()
#   IV.result <- IV.Calc( datas ,side = 20 ,plotdim=c(500,300))
#   time01 <- Sys.time()
#   time01-time00

# 保存结果 IV.result
#   tim_0<- format(Sys.time()," %x%H%M%S")
#   write.csv(IV.result ,paste("NG.IV",tim_0,".csv",sep="")

###################################


