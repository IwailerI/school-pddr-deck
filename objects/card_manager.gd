extends Node


var _cards: Array[Card]

var static_cards := [
	[
		"[center]Самое маленькое натуральное число?",
		"[center]1",
	],
	[
		"[center]Какие числа являются натуральными?",
		"[center]Целые положительные числа.",
	],
	[
		"[center]Чему равна сумма первых 100 натуральных чисел?",
		"[center]5050.",
	],
	[
		"[center]Разложить выражение на множители:\n[img]uid://c420kfdsjxum4[/img]",
		"[center][img]uid://1q1g7dwdvg4b[/img]",
	],
]


# Called when the node enters the scene tree for the first time.
func _ready():
	# generate static_cards
	_cards = []
	for element in static_cards:
		var card := StaticCard.new()
		card.question_text = element[0]
		card.answer_text = element[1]
		_cards.push_back(card)

	# add additional dynamic cards


func get_deck() -> Array[Card]:
	print(_cards)
	return _cards.duplicate()
