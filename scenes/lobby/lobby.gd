extends VBoxContainer


@onready var picker: DeckPicker = %DeckPicker


func _ready():
	picker.deck_selected.connect(_on_deck_selected)

	(%DynamicCardsAmount as SpinBox).value_changed.connect(func (f: float):
		picker.dynamic_count = int(f))

	(%ViewerMode as CheckBox).toggled.connect(func (v: bool):
		%CardNumbers.disabled = v
		%RemainingCards.disabled = v
		%DynamicCardsAmount.editable = not v)


func _on_deck_selected(deck: Deck) -> void:
	var viewer: bool = %ViewerMode.button_pressed
	var card_numbers: bool = %CardNumbers.button_pressed
	var remaining_cards: bool = %RemainingCards.button_pressed
	var dynamic_cards: bool = %UseDynamicCards.button_pressed
	var dynamic_cards_count: int = int(%DynamicCardsAmount.value)

	hide()

	var scene: Node
	if viewer:
		scene = preload("res://scenes/viewer/viewer.tscn").instantiate()
	else:
		scene = preload("res://scenes/simulator/simulator.tscn").instantiate()

	get_tree().root.add_child(scene)
	get_tree().current_scene = scene

	if not scene.is_node_ready():
		await scene.ready

	if viewer:
		scene.set_dynamic_cards(dynamic_cards)
	else:
		scene.dynamic_card_count = dynamic_cards_count
		scene.use_dynamic_cards = dynamic_cards
		scene.show_deck_size = remaining_cards
		scene.set_card_number_visible(card_numbers)

	scene.assign_deck(deck)

	queue_free()
