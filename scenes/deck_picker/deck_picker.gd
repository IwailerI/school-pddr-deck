class_name DeckPicker
extends PanelContainer


signal deck_selected(deck: Deck)


@export var decks: Array[Deck] = []:
	set(v):
		decks = v
		if is_node_ready():
			update_decks()

var dynamic_count: int = 1:
	set(v):
		dynamic_count = v
		if is_node_ready():
			update_decks()

var _button_template: Button = null

@onready var _container: Node = %DeckContainer


func _ready():
	_button_template = _container.get_child(0)
	_container.remove_child(_button_template)
	update_decks()


func update_decks() -> void:
	for child in _container.get_children():
		child.queue_free()

	for deck in decks:
		var node: Button = _button_template.duplicate()
		node.text = ("%s (%s)"
				% [deck.name, deck.get_card_number(dynamic_count)])
		node.icon = deck.icon
		node.pressed.connect(func (): deck_selected.emit(deck))
		_container.add_child(node)
