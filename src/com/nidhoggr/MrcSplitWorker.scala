package com.nidhoggr

import akka.actor.{ActorLogging, Actor}
import java.io.File

object MrcSplitWorker {
  case class SplitMrc(mrcfile: String)
  case class MrcSplitError(msg: String)
  case class MrcSplitOk(topImg: File)
}

class MrcSplitWorker extends Actor with ActorLogging {
  import MrcSplitWorker._

  def receive = {
    case SplitMrc(mrcfile) => {
      val f = new File(context.system.settings.config.getString("nidhoggr.mrcdi") + mrcfile)
      if (!f.exists || f.isDirectory)
        sender ! MrcSplitError("File does not exist")
      else {
        sender ! MrcSplitOk(f)
      }
    }
  }
}
