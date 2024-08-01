package com.github.dunnololda.scage.support

import com.github.dunnololda.mysimplelogger.MySimpleLogger.MySimpleLogger

object XInitThreadsCaller {
  def xInitThreads(scage_log: MySimpleLogger): Unit = {
    if ("sun.awt.X11.XToolkit" == System.getProperty("awt.toolkit")) {
      // in Windows: sun.awt.windows.WToolkit
      if ("32" == System.getProperty("sun.arch.data.model")) {
        scage_log.info("32 bit linux detected, performing XInitThreads() call to make multi-threading work by loading library libxx32.so")
        System.loadLibrary("xx32")
      } else if ("64" == System.getProperty("sun.arch.data.model")) {
        scage_log.info("64 bit linux detected, performing XInitThreads() call to make multi-threading work by loading library libxx64.so")
        System.loadLibrary("xx64")
      } else {
        scage_log.warn("linux of unknown arch detected, don't now how to perform XInitThreads() call, so perform nothing. Maybe the app will crash. Sorry.")
      }
    }
  }
}
