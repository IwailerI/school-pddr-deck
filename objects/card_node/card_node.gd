class_name CardNode
extends PanelContainer


signal clicked

var _cached_answer := ""
@onready var _content: RichTextLabel = %Content
@onready var _number: Label = %Number


## Assigns custom resource data to this card. Negative card number will be
## hidden. [param dynamic] is passed as-is to resource.
## Card answer is cached for future use.
func assign_card(card: Card, dynamic: bool) -> void:
	_content.text = "[center]" + card._get_question_text(dynamic)
	if card.number >= 0:
		_number.text = "#" + str(card.number)
	else:
		_number.text = "#invalid"
	_cached_answer = card._get_answer_text(dynamic)


## Reveals a pre-saved answer. Must be called only after [method assign_card].
func show_answer() -> void:
	_content.text = "[center]" + _cached_answer


func set_number_visible(visibility: bool):
	_number.visible =  visibility


func _gui_input(event: InputEvent):
	if event is InputEventMouseButton:
		if event.button_index == MOUSE_BUTTON_LEFT and event.is_pressed():
			accept_event()
			clicked.emit()
	elif event is InputEventScreenTouch:
		if event.pressed:
			accept_event()
			clicked.emit()
