package org.seekloud.yubel.util

import org.scalajs.dom

import scala.xml.Elem

/**
  * Created by Lty on 18/5/6 4PM
  */

class Modal(header:Elem, child:Elem, successFunc:() => Unit, id:String, withButton: Boolean = true) extends Component {


  val closeBtn = <button data-dismiss="modal" class="btn btn-default" id="closeBtn">关闭</button>
  val confirmBtn = <button data-dismissAll="modal" class="btn btn-primary" id="confirmBtn" onclick={() => successFunc()}>确认</button>

  def noButton: Unit ={
    dom.document.getElementById("closeBtn").setAttribute("style","display:none")
    dom.document.getElementById("confirmBtn").setAttribute("style","display:none")
  }

  val modal =
    <div class="modal fade" id={id}  data-backdrop="static" tabindex="-1" role="dialog" aria-labelledby="myModalLabel">
      <div class="modal-dialog">
        <div class="modal-content">
          <div class="modal-header">
            {header}
          </div>
          <div class="modal-body">
            {child}
          </div>
          <div class="modal-footer">
            {closeBtn}
            {confirmBtn}
          </div>
        </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
    </div>

  val msgModal =
    <div class="modal fade" id={id}  data-backdrop="static" tabindex="-1" role="dialog" aria-labelledby="myModalLabel">
      <div class="modal-dialog">
        <div class="modal-content">
          <div class="modal-header">
            <button data-dismiss="modal" class="close" type="button"><span aria-hidden="true">×</span><span class="sr-only">Close</span></button>
            {header}
          </div>
          <div class="modal-body">
            {child}
          </div>
        </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
    </div>

  override def render: Elem = {
    <div>
      {
      if(withButton) modal
      else msgModal
      }
    </div>
  }
}
