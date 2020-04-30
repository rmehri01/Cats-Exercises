package sandbox.fpinscala.propertytesting

trait Prop {
  def check: Boolean

  def &&(p: Prop): Prop =
    new Prop {
      override def check: Boolean = Prop.this.check && p.check
    }
}
