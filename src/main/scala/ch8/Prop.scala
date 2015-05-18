package ch8

trait Prop {
  def check: Boolean

  // ex8.3
  def &&(p: Prop): Prop =
    new Prop {
      def check = Prop.this.check && p.check
    }
}
