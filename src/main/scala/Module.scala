package gama

abstract class Module[+IOT<:Data](ioDef: IOT){
  val io: IOT = ioDef
}
