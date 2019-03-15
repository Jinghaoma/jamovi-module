
# This file is a generated template, your changes will not be overwritten

IndttestClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "IndttestClass",
    inherit = IndttestBase,
    private = list(
      .run = function() {
        # stan保存模型
        rstan_options(auto_write = TRUE)

        # モデルおパスを得る
        module_name = "jmvstan"
        temporary_name = "Indttest_temporary.RDS"

        stan_path = system.file("stan",package = module_name) #记得改这里的名字
        temporary_path = paste0(stan_path,"/temporary/",temporary_name)

        # map函数
        map <- function(z){
          density(z)$x[which.max(density(z)$y)]
        }

        # MCMCの一般結果処理
        MCMC_process = function(MCMC_object,CI_range,HDI_range=NULL){
          if (is.null(HDI_range)) {
            CI_low_r = (1-CI_range)/2
            CI_up_r = 1-((1-CI_range)/2)
            HPDI = HDInterval::hdi(MCMC_object,CI_range)
          }else{
            CI_low_r = (1-CI_range)/2
            CI_up_r = 1-((1-CI_range)/2)
            HPDI = HDInterval::hdi(MCMC_object,HDI_range)
          }
          ret = list(MAP = map(MCMC_object),
                     EAP = mean(MCMC_object),
                     MED = median(MCMC_object),
                     CI_L = quantile(MCMC_object,CI_low_r),
                     CI_U = quantile(MCMC_object,CI_up_r),
                     HDI_L = HPDI[1],
                     HDI_U = HPDI[2])
          return(ret)
        }

        # MCMC表充填の一般関数
        setTabal = function(table_name,row_num,MCMC_process_object,var_mes = ""){
          table_name$setRow(rowNo=row_num, values=list(
            var = var_mes,
            MAP=MCMC_process_object$MAP,
            EAP=MCMC_process_object$EAP,
            MED=MCMC_process_object$MED,
            CI.Lower=MCMC_process_object$CI_L,
            CI.Upper=MCMC_process_object$CI_U,
            HPDI.Lower=MCMC_process_object$HDI_L,
            HPDI.Upper=MCMC_process_object$HDI_U
          ))
        }

        Add_Tabal_row = function(table_name,rwo_key,MCMC_process_object,var_mes = ""){
          table_name$addRow(rowKey=rwo_key, values=list(
            var = var_mes,
            MAP=MCMC_process_object$MAP,
            EAP=MCMC_process_object$EAP,
            MED=MCMC_process_object$MED,
            CI.Lower=MCMC_process_object$CI_L,
            CI.Upper=MCMC_process_object$CI_U,
            HPDI.Lower=MCMC_process_object$HDI_L,
            HPDI.Upper=MCMC_process_object$HDI_U
          ))
        }
        # 处理数据
        # 提取因子名
        lev = levels(self$data[[self$options$group]])
        # 根据因子名分组
        g1 = subset(self$data,self$data[[self$options$group]]==lev[1])[[self$options$dep]]
        g2 = subset(self$data,self$data[[self$options$group]]==lev[2])[[self$options$dep]]

        # 主列表
        maintable <- self$results$maintable
        # 扳机列表
        trigger <- self$results$triggertable


        # 如果未被填充则执行MCMC
        if (! trigger$isFilled(rowNo=1, col=1)) {

          stan_data = list(N1=length(g1), N2=length(g2),X1=g1,X2=g2)

          # モデルのパスを得る
          model_path = paste0(stan_path,"/model/Independenttest.stan")
          fit = stan(file = model_path,
                     data = stan_data,
                     iter = self$options$iters,
                     warmup = self$options$warmup,
                     chains = self$options$chain,
                     cores = self$options$core,
                     seed = self$options$seed)

          saveRDS(object = fit,file = temporary_path)
        }
        else{
          fit = readRDS(file = temporary_path)
        }

        extract1 = extract(fit)
        summ = summary(fit)$summary

        # 填充扳机列表
        trigger$setRow(rowNo=1, values=list(M=summ[1,1]))

        # 文字版结果
        self$results$text$setContent(fit)

        # 填充主列表
        setR = function(name,row_num,factor_num,Rhat_row_num,Rhat_col_num){
          rest = MCMC_process(extract1[[name]],self$options$Range.CI/100,self$options$Range.HPDI/100)

          maintable$setRow(rowNo=row_num, values=list(
            var=paste0(as.character(lev[factor_num])," (",name,")"),
            MAP=rest$MAP,
            EAP=rest$EAP,
            MED=rest$MED,
            CI.Lower=rest$CI_L,
            CI.Upper=rest$CI_U,
            HPDI.Lower=rest$HDI_L,
            HPDI.Upper=rest$HDI_U,
            Rhat=summ[Rhat_row_num,Rhat_col_num]
          ))
        }

        if (! maintable$isFilled(rowNo=1, col=1)) {
          if(self$options$varEq){
            setR("mu1_same",1,1,5,10)
            setR("mu2_same",2,2,6,10)
            setR("sigma_same",3,1,7,10)
            setR("sigma_same",4,2,7,10)
          }else{
            setR("mu1",row_num = 1,factor_num = 1,Rhat_row_num = 1,Rhat_col_num = 10)
            setR("mu2",2,2,2,10)
            setR("sigma1",3,1,3,10)
            setR("sigma2",4,2,4,10)
          }
        }

        # 检查事后分布 作图
        image <- self$results$acplot
        image$setState(fit)

        #效应量函数
        Effect_size_calculation = function(mu1,mu2,sigma1,sigma2=NULL){
          mean_dif = mu1-mu2
          if(is.null(sigma2)){
            cohen_d = mean_dif/sigma1
          }else{
            # コメントはn-1
            # SE = sqrt(((length(g1)-1)*(extract1$sigma1)^2+(length(g2)-1)*(extract1$sigma2)^2)/(length(g1)+length(g2)-2))
            SE = sqrt((length(g1)*sigma1^2+length(g2)*sigma2^2)/(length(g1)+length(g2)))
            cohen_d = mean_dif/SE
          }
          return(list(mean_dif = mean_dif,cohen_d = cohen_d))
        }

        # 効果量結果充填
        estable <- self$results$EStable

        mean_dif_var = paste0(as.character(lev[1]),"-",as.character(lev[2]))
        cohen_d_var = paste0(mean_dif_var, " (Cohen's d)")
        if(self$options$varEq){
          es_object = Effect_size_calculation(extract1$mu1_same,extract1$mu2_same,extract1$sigma_same)
          if(self$options$mean_diff){
            mean_dif = MCMC_process(MCMC_object = es_object$mean_dif,CI_range = self$options$mean_diff_Range/100,HDI_range = self$options$mean_diff_Range/100)
            Add_Tabal_row(table_name = estable,rwo_key = "mean_dif",MCMC_process_object = mean_dif,var_mes = mean_dif_var)
          }
          if(self$options$ES){
            cohen_d = MCMC_process(MCMC_object = es_object$cohen_d,CI_range = self$options$ES_Range/100,HDI_range = self$options$ES_Range/100)
            Add_Tabal_row(table_name = estable,rwo_key = "cohen_d",MCMC_process_object = cohen_d,var_mes = cohen_d_var)
          }
        }
        else{
          es_object = Effect_size_calculation(extract1$mu1,extract1$mu2,extract1$sigma1,extract1$sigma2)
          if(self$options$mean_diff){
            mean_dif = MCMC_process(MCMC_object = es_object$mean_dif,CI_range = self$options$mean_diff_Range/100,HDI_range = self$options$mean_diff_Range/100)
            Add_Tabal_row(table_name = estable,rwo_key = "mean_dif",MCMC_process_object = mean_dif,var_mes = mean_dif_var)
          }
          if(self$options$ES){
            cohen_d = MCMC_process(MCMC_object = es_object$cohen_d,CI_range = self$options$ES_Range/100,HDI_range = self$options$ES_Range/100)
            Add_Tabal_row(table_name = estable,rwo_key = "cohen_d",MCMC_process_object = cohen_d,var_mes = cohen_d_var)
          }
        }

        #非重複度
        U3 = function(mu1,mu2,sigma1,sigma2=NULL){
          if (is.null(sigma2)){
            # 等分散の場合
            u3_12 = dnorm(mu1,mu2,sigma1)
            u3_21 = 1-dnorm(mu2,mu1,sigma1)
          }else{
            # 非等分散なら
            u3_12 = dnorm(mu1,mu2,sigma2)
            u3_21 = 1-dnorm(mu2,mu1,sigma1)
          }
          return(list(u3_12=u3_12,u3_21=u3_21))
        }

        #優越率
        pid = function(mu1,mu2,sigma1,sigma2=NULL){
          if (is.null(sigma2)) {
            pid = dnorm((mu1-mu2)/sqrt(2*sigma1))
          }else{
            pid = dnorm((mu1-mu2)/sqrt(sigma1+sigma2))
          }
          return(list(pid = pid))
        }

        #閾上率
        pic = function(mu1,mu2,x,sigma1,sigma2=NULL){
          if (is.null(sigma2)) {
            pic = dnorm((mu1-mu2-x)/sqrt(2*sigma1))
          }else{
            pic = dnorm((mu1-mu2-x)/sqrt(sigma1+sigma2))
          }
          return(list(pic = pic))
        }

        PDanalysis <- self$results$PDanalysis
        if(self$options$varEq){
          U3_O = U3(extract1$mu1_same,extract1$mu2_same,extract1$sigma_same)
          pid_O = pid(extract1$mu1_same,extract1$mu2_same,extract1$sigma_same)
          pic_O = pic(extract1$mu1_same,extract1$mu2_same,x = self$options$pi_c_c,extract1$sigma_same)
          if (self$options$U3) {

            u3_12 = MCMC_process(U3_O$u3_12,self$options$Range_U3/100)
            u3_21 = MCMC_process(U3_O$u3_21,self$options$Range_U3/100)

            Add_Tabal_row(table_name =PDanalysis,rwo_key = "U3_12",MCMC_process_object = u3_12,var_mes = "Cohen'U3_12")
            Add_Tabal_row(table_name =PDanalysis,rwo_key = "U3_21",MCMC_process_object = u3_21,var_mes = "Cohen'U3_21")
          }
          if (self$options$pi_d) {
            pid_table = MCMC_process(pid_O$pid,self$options$Range_pi_d/100)
            Add_Tabal_row(table_name =PDanalysis,rwo_key = "pid",MCMC_process_object = pid_table,var_mes = "probability of dominance")
          }
          if (self$options$pi_c) {
            pic_table = MCMC_process(pic_O$pic,self$options$Range_pi_c/100)
            Add_Tabal_row(table_name =PDanalysis,rwo_key = "pic",MCMC_process_object = pic_table,var_mes = "probability beyond threshold")
          }
        }
        else{
          U3_O = U3(extract1$mu1,extract1$mu2,extract1$sigma1,extract1$sigma2)
          pid_O = pid(extract1$mu1,extract1$mu2,extract1$sigma1,extract1$sigma2)
          pic_O = pic(mu1 = extract1$mu1,mu2 = extract1$mu2,x = self$options$pi_c_c,sigma1 = extract1$sigma1,sigma2 = extract1$sigma2)
          if (self$options$U3) {
            u3_12 = MCMC_process(U3_O$u3_12,self$options$Range_U3/100)
            u3_21 = MCMC_process(U3_O$u3_21,self$options$Range_U3/100)

            Add_Tabal_row(table_name =PDanalysis,rwo_key = "U3_12",MCMC_process_object = u3_12,var_mes = "Cohen'U3_12")
            Add_Tabal_row(table_name =PDanalysis,rwo_key = "U3_21",MCMC_process_object = u3_21,var_mes = "Cohen'U3_21")
          }
          if (self$options$pi_d) {
            pid_table = MCMC_process(pid_O$pid,self$options$Range_pi_d/100)
            Add_Tabal_row(table_name =PDanalysis,rwo_key = "pid",MCMC_process_object = pid_table,var_mes = "probability of dominance")
          }
          if (self$options$pi_c) {
            pic_table = MCMC_process(pic_O$pic,self$options$Range_pi_c/100)
            Add_Tabal_row(table_name =PDanalysis,rwo_key = "pic",MCMC_process_object = pic_table,var_mes = "probability beyond threshold")
          }
        }

        #PHC表
        PHCtable <- self$results$PHCtable

        # PHC関数
        phc = function(MCMC_object,x){
          mean(MCMC_object>x)
        }

        if (self$options$mean_diff_over) {
          mean_dif_phc = phc(es_object$mean_dif,self$options$mean_diff_over_c)
          PHCtable$setRow(rowNo=1,values=list(mean_diff_o = mean_dif_phc))
        }
        if (self$options$ES_over) {
          ES_phc = phc(es_object$cohen_d,self$options$ES_over_c)
          PHCtable$setRow(rowNo=1,values=list(ES_o = ES_phc))
        }
        if (self$options$U3_over) {
          U3_phc = phc(U3_O$u3_12,self$options$U3_over_x)
          PHCtable$setRow(rowNo=1,values=list(U3_o = U3_phc))
        }
        if (self$options$pi_d_over) {
          pi_d_phc = phc(pid_O$pid,self$options$pi_d_over_x)
          PHCtable$setRow(rowNo=1,values=list(pi_d_o = pi_d_phc))
        }
        if (self$options$pi_c_over) {
          pi_c_phc = phc(pic_O$pic,self$options$pi_c_over_x)
          PHCtable$setRow(rowNo=1,values=list(pi_c_o = pi_c_phc))
        }



        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)

      },

      .init=function() {
        # 初始化PHC表格
        PHCtable = self$results$PHCtable

        mean_dif_title = paste0("mu1-mu2 > ",self$options$mean_diff_over_c)
        ES_title=paste0("Cohen's d > ",self$options$ES_over_c)
        U3_title=paste0("Cohen'U3_12 > ",self$options$U3_over_x)
        pid_title=paste0("probability of dominance > ",self$options$pi_d_over_x)
        pic_title=paste0("probability beyond threshold > ",self$options$pi_c_over_x)

        PHCtable$getColumn('mean_diff_o')$setTitle(mean_dif_title)
        PHCtable$getColumn('ES_o')$setTitle(ES_title)
        PHCtable$getColumn('U3_o')$setTitle(U3_title)
        PHCtable$getColumn('pi_d_o')$setTitle(pid_title)
        PHCtable$getColumn('pi_c_o')$setTitle(pic_title)

        # 廃棄
        # CI_SuperTitle = function(table_name,CI_mes,HDI_mes){
        #   table_name$getColumn('CI.low')$setSuperTitle(CI_mes)
        #   table_name$getColumn('CI.hight')$setSuperTitle(CI_mes)
        #   table_name$getColumn('HPDI.low')$setSuperTitle(HDI_mes)
        #   table_name$getColumn('HPDI.hight')$setSuperTitle(HDI_mes)
        # }
        #
        # maintable = self$results$maintable
        # ciTitle <- paste0(self$options$Range.CI, '% Confidence Interval')
        # HdiTitle <- paste0(self$options$Range.HPDI, '% Highest Density Interval')
        # CI_SuperTitle(table_name = maintable,ciTitle,HdiTitle)
        #
        # EStable = self$results$EStable
        # ciTitle <- paste0(self$options$Range.CI, '% Confidence Interval')
        # HdiTitle <- paste0(self$options$Range.HPDI, '% Highest Density Interval')
        # CI_SuperTitle(table_name = EStable,ciTitle,HdiTitle)
      },

      .plotrhat=function(image, ...) {
        if(self$options$check_Rhat){
          stan_path = system.file("stan",package = "jmvstan") #记得改这里的名字
          temporary_path = paste0(stan_path,"/temporary/Indttest_temporary.RDS")
          plotData <- readRDS(file = temporary_path)

          plo = rstan::stan_rhat(plotData,bins=10)
          print(plo)
          TRUE
        }else{TRUE}
      },

      .plotac=function(image, ...) {
        if(self$options$check_ac){
          stan_path = system.file("stan",package = "jmvstan") #记得改这里的名字
          temporary_path = paste0(stan_path,"/temporary/Indttest_temporary.RDS")
          plotData <- readRDS(file = temporary_path)

          plo = rstan::stan_ac(plotData)
          print(plo)
          TRUE
        }else{TRUE}
      },

      .plottrace=function(image, ...) {
        if(self$options$check_trace){#改这里的选项
          stan_path = system.file("stan",package = "jmvstan") #记得改这里的名字
          temporary_path = paste0(stan_path,"/temporary/Indttest_temporary.RDS")
          plotData <- readRDS(file = temporary_path)

          plo = rstan::stan_trace(plotData)
          print(plo)#改这里的函数
          TRUE
        }else{TRUE}
      }
    )
)
