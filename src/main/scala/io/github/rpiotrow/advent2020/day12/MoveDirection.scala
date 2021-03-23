package io.github.rpiotrow.advent2020.day12

sealed trait MoveDirection {
  def change(direction: TurnDirection, amount: TurnAmount): MoveDirection =
    direction match {
      case LeftDirection => changeLeft(amount)
      case RightDirection => changeRight(amount)
    }
  def changeLeft(amount: TurnAmount): MoveDirection
  def changeRight(amount: TurnAmount): MoveDirection
}

case object North extends MoveDirection {
  override def changeLeft(amount: TurnAmount): MoveDirection =
    amount match {
      case Degrees90 => West
      case Degrees180 => South
      case Degrees270 => East
    }
  override def changeRight(amount: TurnAmount): MoveDirection =
    amount match {
      case Degrees90 => East
      case Degrees180 => South
      case Degrees270 => West
    }
}
case object South extends MoveDirection {
  override def changeLeft(amount: TurnAmount): MoveDirection =
    amount match {
      case Degrees90 => East
      case Degrees180 => North
      case Degrees270 => West
    }

  override def changeRight(amount: TurnAmount): MoveDirection =
    amount match {
      case Degrees90 => West
      case Degrees180 => North
      case Degrees270 => East
    }
}
case object East extends MoveDirection {
  override def changeLeft(amount: TurnAmount): MoveDirection =
    amount match {
      case Degrees90 => North
      case Degrees180 => West
      case Degrees270 => South
    }

  override def changeRight(amount: TurnAmount): MoveDirection =
    amount match {
      case Degrees90 => South
      case Degrees180 => West
      case Degrees270 => North
    }
}
case object West extends MoveDirection {
  override def changeLeft(amount: TurnAmount): MoveDirection =
    amount match {
      case Degrees90 => South
      case Degrees180 => East
      case Degrees270 => North
    }

  override def changeRight(amount: TurnAmount): MoveDirection =
    amount match {
      case Degrees90 => North
      case Degrees180 => East
      case Degrees270 => South
    }
}
