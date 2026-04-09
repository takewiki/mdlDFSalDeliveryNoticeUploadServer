
#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' salDeliveryNoticeUploadServer()
salDeliveryNoticeUploadServer <- function(input,output,session,dms_token) {


  options(shiny.maxRequestSize = 30 * 1024^2)
  #获取参数
  text_salDeliveryNotice_upload = tsui::var_file('text_salDeliveryNotice_upload')

  shiny::observeEvent(input$btn_salDeliveryNotice_upload,{

    filename=text_salDeliveryNotice_upload()

    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")


    }else{

      # 清空临时表

      mdlDFSalDeliveryNoticeUploadPkg::salDeliveryNotice_delete(dms_token = dms_token)


      data <- readxl::read_excel(filename, col_types =  c("text", "text", "text",
                                                          "text", "text", "text", "text", "text",
                                                          "text", "text", "text", "numeric",
                                                          "text", "text", "text", "text", "text",
                                                          "text", "text", "text", "text", "numeric",
                                                          "numeric"))


      data = as.data.frame(data)
      data = tsdo::na_standard(data)





      tsda::mysql_writeTable2(token = dms_token,table_name = 'rds_erp_byd_src_t_sal_DeliveNotice_list_input',r_object = data,append = TRUE)

      # 插入list表和表头表体

      mdlDFSalDeliveryNoticeUploadPkg::salDeliveryNotice_insert(dms_token = dms_token)

      tsui::pop_notice("上传成功")


    }


  })



}



#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' salDeliveryNoticeViewServer()
salDeliveryNoticeViewServer <- function(input,output,session,dms_token) {

  #获取参数
  text_salDeliveryNotice_daterange = tsui::var_dateRange('text_salDeliveryNotice_daterange')

  shiny::observeEvent(input$btn_salDeliveryNotice_view,{

    FDate = text_salDeliveryNotice_daterange()

    FStartDate = FDate[1]

    FEndDate = FDate[2]

    data = mdlDFSalDeliveryNoticeUploadPkg::salDeliveryNotice_select(dms_token = dms_token,FStartDate =FStartDate ,FEndDate = FEndDate)


    tsui::run_dataTable2(id = 'salDeliveryNotice_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_salDeliveryNotice',data = data,filename = 'BYD发货通知单.xlsx')




  })



}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' salDeliveryNoticeServer()
salDeliveryNoticeServer <- function(input,output,session,dms_token) {

  salDeliveryNoticeUploadServer(input = input,output = output,session = session,dms_token = dms_token)



  salDeliveryNoticeViewServer(input = input,output = output,session = session,dms_token = dms_token)


}
