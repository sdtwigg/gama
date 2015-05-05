package gama

sealed trait WarnLevel {def tf: Boolean}
case object Succeed    extends WarnLevel {def tf = true}
case object SuceedWarn extends WarnLevel {def tf = true}
case object FailWarn   extends WarnLevel {def tf = false}
case object FailError  extends WarnLevel {def tf = false}

