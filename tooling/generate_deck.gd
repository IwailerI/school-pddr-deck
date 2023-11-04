@tool
extends EditorScript

const INPUT_PATH: String = "res://tooling/deck.json"
const OUTPUT_PATH: String = "res://resources/decks/prod_ru_v2.tres"
const RESOURCE_OUTPUT: String = "res://assets/equations/"
const MAX_WIDTH: int = 660


var miss := {}

func _run():
	var file_in := FileAccess.open(INPUT_PATH, FileAccess.READ)

	var data: Array = JSON.parse_string(file_in.get_as_text())

	var cards: Array[StaticCard] = []

	for card_tupple in data:
		var card := StaticCard.new()
		card.question_text = parse_rich_text(card_tupple[0])
		card.answer_text = parse_rich_text(card_tupple[1])
		cards.push_back(card)

	if miss.size() > 0:
		printerr("Missed %d latex equations!" % miss.size())
		print(miss.keys())

	create_deck(cards)
	print("Created deck with %s cards." % cards.size())


func parse_rich_text(text: Array) -> String:
	var res := ""
	for part in text:
		if part[0] == "text":
			res += part[1]
		elif part[0] == "latex":
			var uid := ResourceLoader.get_resource_uid(RESOURCE_OUTPUT + part[1] + ".png")
			if uid == -1:
				miss[part[1]] = true
				res += "[img]%s[/img]" % ResourceUID.id_to_text(-1)
				continue

			var image: Texture2D = load(ResourceUID.id_to_text(uid))
			if image.get_width() > MAX_WIDTH:
				res += "[img width=%s]%s[/img]" % [MAX_WIDTH, ResourceUID.id_to_text(uid)]
			else:
				res += "[img]%s[/img]" % ResourceUID.id_to_text(uid)
		else:
			assert(false, "idk bro")
	return res


func create_deck(data: Array[StaticCard]) -> Deck:

	var d: Deck
	if ResourceLoader.exists(OUTPUT_PATH):
		d = load(OUTPUT_PATH)
		print("Loading existing deck")
	else:
		d = Deck.new()
		d.name = "Auto generated deck"
		d.take_over_path(OUTPUT_PATH)
		print("Creating new deck")

	d.static_cards = data

	ResourceSaver.save(d, OUTPUT_PATH)

	return d
