class_name Card
extends Resource


## Number of this card in deck. Determined automatically.
var number: int = -1


## Virtual. Method to get text of the question. Supports bbcode.
func _get_question_text(_dynamic: bool = true) -> String:
	assert(false, "not implemented")
	return "not implemented"


## Virtual. Method to get text of the answer. Supports bbcode.
func _get_answer_text(_dynamic: bool = true) -> String:
	assert(false, "not implemented")
	return "not implemented"
