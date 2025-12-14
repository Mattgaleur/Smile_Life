package apps.ul2025app68

import org.scalacheck.{Arbitrary, Gen}
import cs214.webapp.UserId

val positiveInt: Gen[Int] = Gen.choose(1, 10_000)
val smallInt: Gen[Int]    = Gen.choose(0, 10)
val smallSize: Gen[Int] = Gen.choose(0, 10)

given Arbitrary[Malus] =
  Arbitrary(Gen.oneOf(Malus.values.toSeq))

given Arbitrary[Bonus] =
  Arbitrary {
    val simpleBonuses = Gen.oneOf(
      Bonus.FreeHouse,
      Bonus.FreeTravel,
      Bonus.UnlimitedFlirt,
      Bonus.FlirtWhileMarried,
      Bonus.UnlimitedStudy,
      Bonus.StudyWhileWorking,
      Bonus.DoubleStudy,
      Bonus.DoubleMarriage
    )

    val malusProtection =
      Arbitrary.arbitrary[Malus].map(Bonus.MalusProtection.apply)

    Gen.oneOf(simpleBonuses, malusProtection)
  }


val genBonusSeq: Gen[Option[Seq[Bonus]]] =
  Gen.option(Gen.nonEmptyListOf(Arbitrary.arbitrary[Bonus]))

given Arbitrary[Card] =
  Arbitrary {
    val simpleCards = Gen.oneOf(
      Card.Flirt,
      Card.Marriage,
      Card.Child,
      Card.Study,
      Card.Pet
    )

    val malusCard =
      Arbitrary.arbitrary[Malus].map(Card.MalusCard.apply)

    val travel =
      positiveInt.map(Card.Travel.apply)

    val house =
      positiveInt.map(Card.House.apply)

    val special =
      for
        bonus <- Arbitrary.arbitrary[Bonus]
        name  <- Gen.alphaStr.suchThat(_.nonEmpty)
      yield Card.Special(bonus, name)

    val money =
      for
        amount <- positiveInt
        used   <- Gen.oneOf(true, false)
      yield Card.Money(amount, used)

    val profession =
      for
        studyRequired <- smallInt
        salary        <- positiveInt
        bonus         <- genBonusSeq
        name          <- Gen.alphaStr.suchThat(_.nonEmpty)
      yield Card.Profession(studyRequired, salary, bonus, name)

    Gen.oneOf(
      simpleCards,
      malusCard,
      travel,
      house,
      special,
      money,
      profession
    )
  }

given Arbitrary[PhaseView] =
  Arbitrary {

    val gameView =
      for
        board         <- Arbitrary.arbitrary[Board]
        hand          <- Arbitrary.arbitrary[Hand]
        lastDiscard   <- Gen.option(Arbitrary.arbitrary[Card])
        turnOf        <- Arbitrary.arbitrary[UserId]
        drawPileSize  <- Gen.choose(0, 200)
        log           <- Arbitrary.arbitrary[Log]
      yield PhaseView.GameView(
        board,
        hand,
        lastDiscard,
        turnOf,
        drawPileSize,
        log
      )

    val victoryView =
      Gen.nonEmptyListOf(Arbitrary.arbitrary[UserId])
        .map(_.distinct)
        .map(PhaseView.VictoryView.apply)

    Gen.oneOf(gameView, victoryView)
  }

given Arbitrary[Vector[Card]] =
  Arbitrary {
    Gen.choose(0, 10).flatMap { n =>
      Gen.containerOfN[Vector, Card](n, Arbitrary.arbitrary[Card])
    }
  }


given Arbitrary[Pile] =
  Arbitrary {
    smallSize.flatMap { n =>
      Gen.listOfN(n, Arbitrary.arbitrary[Card])
    }
  }

given Arbitrary[Board] =
  Arbitrary {
    for
      size  <- Gen.choose(0, 4) // typical player count
      users <- Gen.containerOfN[Set, UserId](size, Arbitrary.arbitrary[UserId])
      hands <- Gen.listOfN(users.size, Arbitrary.arbitrary[PlayedHand])
    yield users.zip(hands).toMap
  }

given Arbitrary[UserId] =
  Arbitrary {
    Gen.choose(1, 8).map(i => s"user$i")
  }

given Arbitrary[Log] =
  Arbitrary {
    Gen.choose(0, 20).flatMap { n =>
      Gen.listOfN(n, Gen.alphaStr.suchThat(_.nonEmpty))
    }
  }
