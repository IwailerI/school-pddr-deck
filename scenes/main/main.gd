extends Control


enum State {
	NO_DECK,
	NO_DECK_OPTIONS,
	PLAYING,
	PLAYING_SWITCHER,
	PLAYING_OPTIONS,
	VIEWING,
	VIEWING_SWITCHER,
	VIEWING_OPTIONS,
}

enum CardState {
	NOT_DRAWN,
	QUESTION,
	ANSWER,
	NO_CARDS,
}

var state := State.NO_DECK

## Deck that is selected (this is not modified by drawing cards)
var selected_deck: Deck = null
## Cards left in current deck.
## PLAYING: (this is modified by drawing cards)
## Top card (cards_left.back()) is displayed to the user.
## VIEWING: this is unmodified immutable value, to which index points.
var cards_left: Array[Card] = []

## Whether top card in the deck was already flipped by user.
var was_flipped: bool = false

## VIEWING ONLY
## Index of the viewed card.
var viewed_index: int = -1

## OPTIONS
var show_card_numbers: bool
var show_deck_size: bool
var use_dynamic_cards: bool
var dynamic_count: int

# Header
@onready var cards_left_label: Label = %CardsLeftLabel
@onready var card_number_spinbox: SpinBox = %CardNumber

# Tabs for center screen
@onready var card_node: CardNode = %Card
@onready var deck_picker: DeckPicker = %DeckPicker
@onready var options: Control = %Options

# All options
@onready var checkbox_card_numbers: CheckBox = %OptionCardNumbers
@onready var checkbox_viewer_mode: CheckBox = %OptionViewerMode
@onready var checkbox_deck_size: CheckBox = %OptionDeckSize
@onready var checkbox_dynamic: CheckBox = %OptionDynamic
@onready var spinbox_dynamic_count: SpinBox = %OptionDynamicCount
@onready var close_options_button: Button = %ButtonCloseOptions

# Footer
@onready var button_left: Button = %ButtonLeft
@onready var button_right: Button = %ButtonRight
@onready var button_reset: Button = %ButtonReset
@onready var button_decks: Button = %ButtonDecks
@onready var button_settings: Button = %ButtonSettings


func _ready() -> void:
	# sync settings
	show_card_numbers = checkbox_card_numbers.button_pressed
	show_deck_size = checkbox_deck_size.button_pressed
	use_dynamic_cards = checkbox_dynamic.button_pressed
	dynamic_count = int(spinbox_dynamic_count.value)
	checkbox_card_numbers.toggled.connect(func (v):
		card_node.set_number_visible(v)
		show_card_numbers = v)
	checkbox_deck_size.toggled.connect(func (v): show_deck_size = v)
	checkbox_dynamic.toggled.connect(func (v): use_dynamic_cards = v)
	spinbox_dynamic_count.value_changed.connect(func (f): dynamic_count = int(f))

	# connect signals
	button_left.pressed.connect(_left_pressed)
	button_right.pressed.connect(_right_pressed)
	button_reset.pressed.connect(_reset_pressed)
	button_settings.pressed.connect(_settings_pressed)
	button_decks.pressed.connect(_decks_pressed)
	card_number_spinbox.value_changed.connect(_index_changed)
	deck_picker.deck_selected.connect(_deck_switched)
	deck_picker.deck_selection_canceled.connect(_close_decks)
	close_options_button.pressed.connect(_close_options)
	card_node.clicked.connect(_card_pressed)

	_update_visual()


func _update_visual() -> void:
	card_node.hide()
	deck_picker.hide()
	options.hide()
	button_left.hide()
	button_right.hide()
	button_reset.hide()
	button_decks.hide()
	button_settings.hide()
	cards_left_label.hide()
	card_number_spinbox.hide()
	match state:
		State.NO_DECK:
			deck_picker.show()
			button_settings.show()
		State.NO_DECK_OPTIONS:
			options.show()
		State.PLAYING:
			card_node.show()
			button_reset.show()
			button_settings.show()
			button_decks.show()
			if show_deck_size:
				cards_left_label.show()
				cards_left_label.text = str(cards_left.size()) + " left"
		State.PLAYING_SWITCHER:
			deck_picker.show()
			button_reset.show()
			button_settings.show()
			button_decks.show()
			if show_deck_size:
				cards_left_label.show()
				cards_left_label.text = str(cards_left.size()) + " left"
		State.PLAYING_OPTIONS:
			options.show()
			button_reset.show()
			button_settings.show()
			button_decks.show()
			if show_deck_size:
				cards_left_label.show()
				cards_left_label.text = str(cards_left.size()) + " left"
		State.VIEWING:
			card_node.show()
			button_decks.show()
			button_settings.show()
			button_left.show()
			button_right.show()
			cards_left_label.show()
			cards_left_label.text = "/" + str(cards_left.size())
			card_number_spinbox.show()
			card_number_spinbox.set_value_no_signal(viewed_index + 1)
		State.VIEWING_SWITCHER:
			deck_picker.show()
			button_decks.show()
			button_settings.show()
			button_left.show()
			button_right.show()
			cards_left_label.show()
			cards_left_label.text = "/" + str(cards_left.size())
			card_number_spinbox.show()
			card_number_spinbox.set_value_no_signal(viewed_index + 1)
		State.VIEWING_OPTIONS:
			options.show()
			button_decks.show()
			button_settings.show()
			button_left.show()
			button_right.show()
			cards_left_label.show()
			cards_left_label.text = "/" + str(cards_left.size())
			card_number_spinbox.show()
			card_number_spinbox.set_value_no_signal(viewed_index + 1)


func _gui_input(event: InputEvent):
	if state == State.VIEWING:
		if event.is_action_pressed("ui_right"):
			accept_event()
			_right_pressed()
		elif event.is_action_pressed("ui_left"):
			accept_event()
			_left_pressed()
	elif state == State.PLAYING:
		if event.is_action_pressed("ui_accept") or event.is_action_pressed("ui_select"):
			accept_event()
			_card_pressed()


func _card_pressed() -> void:
	match state:
		State.PLAYING:
			if was_flipped:
				# get next card
				cards_left.pop_back()
				if cards_left.size() == 0:
					_reset_pressed()
				card_node.assign_card(cards_left.back(), use_dynamic_cards)
				was_flipped = false
				_update_visual()
			else:
				# flip this card
				card_node.show_answer()
				was_flipped = true
		State.VIEWING:
			if was_flipped:
				card_node.assign_card(cards_left[viewed_index], use_dynamic_cards)
			else:
				card_node.show_answer()
			was_flipped = !was_flipped
		_:
			assert(false, "unreachable")


func _left_pressed() -> void:
	_index_changed(viewed_index - 1)


func _right_pressed() -> void:
	_index_changed(viewed_index + 1)


func _index_changed(v: float) -> void:
	assert(state in [State.VIEWING, State.VIEWING_OPTIONS, State.VIEWING_SWITCHER],
			"invalid state")

	viewed_index = clampi(int(v), 1, cards_left.size()) - 1
	card_node.assign_card(cards_left[viewed_index], use_dynamic_cards)
	was_flipped = false
	_update_visual()


func _reset_pressed() -> void:
	assert(state in [State.PLAYING, State.PLAYING_OPTIONS, State.PLAYING_SWITCHER],
			"invalid state")
	cards_left = selected_deck.get_deck(dynamic_count)
	cards_left.shuffle()
	was_flipped = true
	state = State.PLAYING
	_update_visual()


func _settings_pressed() -> void:
	match state:
		State.PLAYING, State.PLAYING_SWITCHER:
			state = State.PLAYING_OPTIONS
		State.PLAYING_OPTIONS:
			state = State.PLAYING
		State.VIEWING, State.VIEWING_SWITCHER:
			state = State.VIEWING_OPTIONS
		State.VIEWING_OPTIONS:
			state = State.VIEWING
		State.NO_DECK:
			state = State.NO_DECK_OPTIONS
		_:
			assert(false, "invalid state")
			return
	_update_visual()


func _decks_pressed() -> void:
	match state:
		State.PLAYING, State.PLAYING_OPTIONS:
			state = State.PLAYING_SWITCHER
		State.VIEWING, State.VIEWING_OPTIONS:
			state = State.VIEWING_SWITCHER
		State.NO_DECK_OPTIONS:
			state = State.NO_DECK
		_:
			assert(false, "invalid state")
			return
	_update_visual()


func _close_options() -> void:
	match state:
		State.PLAYING_OPTIONS:
			state = State.PLAYING
		State.VIEWING_OPTIONS:
			state = State.VIEWING
		State.NO_DECK_OPTIONS:
			state = State.NO_DECK
		_:
			assert(false, "invalid state")
	_update_visual()


func _close_decks() -> void:
	match state:
		State.PLAYING_SWITCHER:
			state = State.PLAYING
		State.VIEWING_SWITCHER:
			state = State.VIEWING
		State.NO_DECK:
			pass
		_:
			assert(false, "invalid state")
	_update_visual()


func _deck_switched(deck: Deck) -> void:
	match state:
		State.PLAYING_SWITCHER:
			selected_deck = deck
			state = State.PLAYING
			_reset_pressed()
		State.VIEWING_SWITCHER:
			selected_deck = deck
			cards_left = deck.get_deck(1)
			state = State.VIEWING
			_reset_pressed()
		State.NO_DECK:
			state = (
				State.VIEWING_SWITCHER if checkbox_viewer_mode.button_pressed
				else State.PLAYING_SWITCHER)
			_deck_switched(deck)
		_:
			assert(false, "invalid state")
