package com.nidhoggr

import com.nidhoggr.NidhoggrPipeline.{PipelineResult, PipelineMsg, PipelineFunction}

object NidhoggrPipeline {
  type Task = (String, String)
  type Image = List[List[Int]]
  type PipelineMsg = (Option[(Image, Image)], Task)
  type PipelineFunction = PipelineMsg => PipelineMsg
  type PipelineResult = (Option[NidhoggrPipeline], PipelineMsg)
}

abstract class NidhoggrPipeline(pipe: List[PipelineFunction]) {
  def apply(msg: PipelineMsg): PipelineResult
}