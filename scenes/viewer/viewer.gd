extends HBoxContainer


var _deck: Array[Card] = []
var _current_index: int = -1
var _answer_shown: bool = false

@onready var card_number: SpinBox = %CardNumber
@onready var deck_size: Label = %DeckSize
@onready var card_node: CardNode = %Card

@onready var button_left: Button = %ButtonLeft
@onready var button_right: Button = %ButtonRight
@onready var button_exit: Button = %ButtonExit
@onready var preview_dynamic: CheckBox = %CheckBoxDynamic


func _ready():
	# move index
	button_left.pressed.connect(func (): _view_index(_current_index - 1))
	button_right.pressed.connect(func (): _view_index(_current_index + 1))
	card_number.value_changed.connect(func (f: float):
		_view_index(int(f) - 1)
		var f_owner := get_viewport().gui_get_focus_owner()
		if is_instance_valid(f_owner):
			f_owner.release_focus())


	# view question when preview_dynamic toggled
	preview_dynamic.toggled.connect(func (_v):
		_answer_shown = true
		_card_pressed())

	button_exit.pressed.connect(func():
		get_tree().change_scene_to_file("res://scenes/lobby/lobby.tscn"))

	card_node.gui_input.connect(_handle_clicked_input)
	card_node.set_number_visible(true)


func _unhandled_input(event: InputEvent):
	if event.is_action_pressed("ui_accept") or event.is_action_pressed("ui_select"):
		accept_event()
		_card_pressed()


func assign_deck(deck: Deck) -> void:
	_deck = deck.get_deck(1)
	_view_index(0)
	_update_header()


func set_dynamic_cards(dynamic_cards: bool) -> void:
	preview_dynamic.set_pressed_no_signal(dynamic_cards)


func _handle_clicked_input(event: InputEvent) -> void:
	if event is InputEventMouseButton:
		if event.button_index == MOUSE_BUTTON_LEFT and event.is_pressed():
			accept_event()
			_card_pressed()
#	elif event is InputEventScreenTouch:
#		if event.is_pressed():
#			accept_event()
#			_card_pressed()


func _card_pressed() -> void:
	_answer_shown = not _answer_shown
	if _answer_shown:
		card_node.show_answer()
	else:
		_view_index(_current_index)


func _view_index(index: int) -> void:
	_current_index = clampi(index, 0, _deck.size()-1)
	card_node.assign_card(_deck[_current_index], preview_dynamic.button_pressed)
	_answer_shown = false
	_update_header()


func _update_header() -> void:
	deck_size.text = "/" + str(_deck.size())
	card_number.max_value = _deck.size()
	card_number.set_value_no_signal(float(_current_index + 1))

