package org.seekloud.yubel.paperClient

import org.seekloud.yubel.util.Component

import scala.xml.Elem

class CanvasPage extends Component{

  override def render: Elem = {
    <div>
      <canvas id="RankView" tabindex="1" style="z-index: 3;position: absolute;"></canvas>
      <canvas id="GameView" tabindex="1" style="position: relative;"></canvas>
      <canvas id="BorderView" tabindex="1"></canvas>
    </div>
  }//borderView style="position: relative;"

}
