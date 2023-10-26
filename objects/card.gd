extends PanelContainer


var _cached_answer := ""


func assign_card(card: Card, number := -1) -> void:
	%Content.text = card._get_question_text()
	if number > 0:
		%Number.text = "#" + str(number)
	%Number.visible = number > 0


func show_answer() -> void:
	%Content.text = _cached_answer
