class_name StaticCard
extends Card


@export_multiline var question_text: String = "Lorem ipsum"
@export_multiline var answer_text: String = "Lorem ipsum"


func _get_question_text() -> String:
	return question_text


func _get_answer_text() -> String:
	return answer_text
