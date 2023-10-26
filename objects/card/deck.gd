class_name Deck
extends Resource


@export var name: String = "A Deck"
@export var icon: Texture
@export var static_cards: Array[StaticCard] = []
@export var dynamic_cards: Array[Card] = []


func get_deck(dynamic_count: int) -> Array[Card]:
	var cards: Array[Card] = []
	cards.assign(static_cards.duplicate())

	for i in dynamic_count:
		cards.append_array(dynamic_cards)

	for i in cards.size():
		cards[i].number = i + 1

	return cards


func get_card_number(dynamic_count: int) -> int:
	return static_cards.size() + dynamic_cards.size() * dynamic_count
