package com.github.dunnololda.scage.support

import com.github.dunnololda.scage.ScageLib._

object ScageColorTest extends ScageScreenApp("Color Test") {
  val fields = ScageColor.getClass.getDeclaredFields
  val colors = fields.map(f => {
    f.setAccessible(true)
    try{f.get(ScageColor).asInstanceOf[ScageColor]}
    catch {
      case ex:Exception => WHITE
    }
  })

  var color_num = 1
  interface {
    if(color_num >= 0 && color_num < fields.length) {
      print(colors(color_num), 20, windowHeight/2,
        if("BLACK".equalsIgnoreCase(fields(color_num).getName)) WHITE else BLACK)
      try {backgroundColor = (colors(color_num))}
      catch {
        case ex:java.lang.Exception =>
      }
    }
  }

  key(KEY_LEFT, onKeyDown = {
    def nextColorNumInc() {
      if(color_num < fields.length - 1) color_num += 1
      else color_num = 0
      if(colors(color_num) != null && (!WHITE.equals(colors(color_num)) || "WHITE".equalsIgnoreCase(fields(color_num).getName))) color_num
      else nextColorNumInc()
    }
    nextColorNumInc()
  })
  key(KEY_RIGHT, onKeyDown = {
    def nextColorNumDec() {
      if(color_num > 0) color_num -= 1
      else color_num = fields.length - 1
      if(!WHITE.equals(colors(color_num)) || "WHITE".equalsIgnoreCase(fields(color_num).getName)) color_num
      else nextColorNumDec()
    }
    nextColorNumDec()
  })
  key(KEY_ESCAPE, onKeyDown = stop())
}