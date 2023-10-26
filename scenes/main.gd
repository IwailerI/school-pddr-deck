extends Control


var deck: Array[Card] = []


# Called when the node enters the scene tree for the first time.
func _ready():
	$Buttons/Draw.pressed.connect(func ():
		%Card.assign_card(deck.pop_front()))
	$Buttons/Reset.pressed.connect(reset)

	reset()


func reset() -> void:
	deck = CardManager.get_deck()
	deck.shuffle()
