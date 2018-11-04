implicit class StringExtension(s:String){
  def dameDos:String = s+s
  def dameFabi:String = "fabi"

}
implicit class StringEx2(s:String){
  def unary_! = s.toUpperCase
  //def unary_! : String = "pepe"+s+"pepe"
  def unary_- : String = "#"+s+"#"


}

println("my string".dameDos)

println("gonzalo".dameFabi)

//println(!"gonzalo")

val v = "gonzalo"
println(! v)

println(- v)

val p = List(1,2,3,4)
p.zip(p.tail)

//# "gonzalo"

