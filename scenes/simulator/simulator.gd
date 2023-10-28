extends Control


enum CardState {
	NOT_DRAWN,
	QUESTION,
	ANSWER,
}

var dynamic_card_count: int
var use_dynamic_cards: bool
var show_deck_size: bool = true

var _deck_template: Deck = null
var _deck: Array[Card] = []
var _card_state := CardState.NOT_DRAWN

# Header
@onready var cards_left_label: Label = %CardsLeftLabel

# Tabs for center screen
@onready var card_node: CardNode = %Card
@onready var empty_state: Control = %EmptyState
@onready var empty_deck: Control = %EmptyDeck

# Footer
@onready var button_reset: Button = %ButtonReset
@onready var button_exit: Button = %ButtonExit


func _ready() -> void:
	card_node.gui_input.connect(_handle_clicked_input)
	empty_state.gui_input.connect(_handle_clicked_input)
	empty_deck.gui_input.connect(_handle_clicked_input)

	button_reset.pressed.connect(_reset)
	button_exit.pressed.connect(func():
		get_tree().change_scene_to_file("res://scenes/lobby/lobby.tscn"))


func assign_deck(deck: Deck) -> void:
	_deck_template = deck
	_reset()


func set_card_number_visible(visibility: bool) -> void:
	card_node.set_number_visible(visibility)


func _gui_input(event: InputEvent):
	if event.is_action_pressed("ui_accept") or event.is_action_pressed("ui_select"):
		accept_event()
		_card_pressed()


func _handle_clicked_input(event: InputEvent) -> void:
	if event is InputEventMouseButton:
		if event.button_index == MOUSE_BUTTON_LEFT and event.is_pressed():
			accept_event()
			_card_pressed()
#	elif event is InputEventScreenTouch:
#		if event.is_pressed():
#			accept_event()
#			_card_pressed()


func _reset() -> void:
	card_node.hide()
	empty_state.show()
	empty_deck.hide()

	_deck = _deck_template.get_deck(dynamic_card_count)
	_deck.shuffle()
	_card_state = CardState.NOT_DRAWN
	_update_cards_left_label()


func _card_pressed() -> void:
	if _deck.is_empty():
		_reset()
	else:
		match _card_state:
			CardState.NOT_DRAWN:
				_draw_card()
			CardState.QUESTION:
				_flip_card()
			CardState.ANSWER:
				_discard_card()
	_update_cards_left_label()


func _draw_card() -> void:
	# TODO: animation
	empty_state.hide()
	empty_deck.hide()
	card_node.show()
	card_node.assign_card(_deck.back(), use_dynamic_cards)
	_card_state = CardState.QUESTION


func _flip_card() -> void:
	card_node.show_answer()
	_card_state = CardState.ANSWER


func _discard_card() -> void:
	_deck.pop_back()
	card_node.hide()
	if _deck.is_empty():
		empty_deck.show()
	else:
		empty_state.show()
	_card_state = CardState.NOT_DRAWN


func _update_cards_left_label() -> void:
	cards_left_label.visible = show_deck_size
	cards_left_label.text = str(_deck.size()) + " left"
