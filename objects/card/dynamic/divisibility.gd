class_name CardIsDivisible
extends Card


var is_divisible := false


func _get_question_text(dynamic: bool = true) -> String:
	if dynamic:
		var a: int = randi_range(100, 200)
		var b: int = [2, 3, 6, 9, 4].pick_random()
		is_divisible = a % b == 0
		return "Делится ли %s на %s" % [a, b]
	else:
		return "Делится ли {DICE} на 6?"


func _get_answer_text(dynamic: bool = true) -> String:
	if dynamic:
		if is_divisible:
			return "Да"
		else:
			return "Нет"
	else:
		return "Я хз бро"
