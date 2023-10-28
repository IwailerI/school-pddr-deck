@tool
extends EditorScript

const INPUT_PATH: String = "res://tooling/input.txt"
const OUTPUT_PATH: String = "res://tooling/output.tres"
const RESOURCE_OUTPUT: String = "res://tooling/resources/"

var block_regex := RegEx.create_from_string("\\$\\$.+\\$\\$")
var line_regex := RegEx.create_from_string("\\$(?:[^$\\\\]|\\\\.)+\\$")

func _run():
	# parse input file
	# format is very simple:
	# blank lines are ignored
	# every line before first Q: or A: is also ignored
	# every question must start with Q:
	# every answer must start with A:
	# whitespace is preserved
	# LATEX is preserved due to magic!

	printerr("THIS TOOL SCRIPT IS NOT FINISHED AND NOT TESTED! PLEASE THINK AGAIN!")
	return

	var file_in := FileAccess.open(INPUT_PATH, FileAccess.READ)
	var raw_strings := parse_input(file_in.get_as_text(true))

	var latex := extract_latex(raw_strings)

	var latex_map := {}
	var i := 0
	for string in latex:
		var path := "res://tooling/resources/equation_%02d.png" % i
		latex_map[string] = path
		render_latex(string, path)

	get_editor_interface().get_resource_filesystem().scan()

	for key in latex_map:
		var id := ResourceUID.create_id()
		ResourceUID.add_id(id, latex_map[key])
		latex_map[key] = ResourceUID.id_to_text(ResourceUID.create_id())

	raw_strings = substitue_latex(raw_strings, latex_map)
	var card_data := create_resources(raw_strings)
	var deck := create_deck(card_data)

	print("Done!")


## Parses input and returns array of strings starting with either
## Q: for questions
## or A: for answers.
func parse_input(input: String) -> Array[String]:
	var res: Array[String] # contains strings with Q: or A: in the beggining
	for line in input.split("\n", false):
		if line.begins_with("Q: ") or line.begins_with("A: "):
			res.push_back(line.strip_edges())
		elif not res.is_empty():
			res[-1] += "\n" + line.strip_edges()

	return res


## Extracts all latex sequences from strings, dedupes them, doesn't strip $
func extract_latex(from: Array[String]) -> Array[String]:
	var formulas := {}
	for string in from:
		# extract blocks:
		for res in block_regex.search_all(string):
			formulas[res.strings[0]] = true
		for res in line_regex.search_all(string):
			formulas[res.strings[0]] = true

	return formulas.keys()


## Renders latex equation (not in math mode btw, still needs $) and returns
## saved .svg path.
func render_latex(latex: String, path: String):
	# klatexformula -l 'x^2+y^2' -o 'file.svg' -f '#ffffff' -m '...'
	# klatexformula -l "$x^2=y^2$" -o file2.png -f "#ffffff" -X 250 -m "..."
	var output := []
	var exit_code := OS.execute("klatexformula", [
		"-l", latex,
		"-o", ProjectSettings.globalize_path(path),
		"-f", "#ffffff",
		"-X", "250",
		"-m", "..."], output, true)

	if exit_code != 0:
		printerr("Latex render failed with exit code %s:\n%s"
				% [exit_code, output[0]])


## Replaces all latex equations with bbcode [img] tags
## latex_map should contain uuid://... paths for each latex equation
func substitue_latex(data: Array[String], latex_map: Dictionary) -> Array[String]:
	var res: Array[String] = []
	for string in data:
		for m in block_regex.search_all(string):
			string = (
				string
				.erase(m.get_start(), m.strings[0].length())
				.insert(m.get_start(), "[img]%s[/img]" % latex_map[m.strings[0]])
			)
		for m in line_regex.search_all(string):
			string = (
				string
				.erase(m.get_start(), m.strings[0].length())
				.insert(m.get_start(), "[img]%s[/img]" % latex_map[m.strings[0]])
			)
		res.push_back(string)
	return res


func create_resources(data: Array[String]) -> Array[StaticCard]:
	var res: Array[StaticCard] = []
	var i := 0
	while i < data.size():
		while not data[i].begins_with("Q"):
			printerr("\"%s\" is not a valid question!" % data[i].c_escape())
			i += 1

		assert(data[i+1].begins_with("A"), "not a valid answer!")

		var card := StaticCard.new()
		card.question_text = data[i].substr(2).strip_edges()
		card.answer_text = data[i+1].substr(2).strip_edges()
		res.push_back(card)
		i += 2

	return res


func create_deck(data: Array[StaticCard]) -> Deck:
	var d := Deck.new()
	d.name = "Auto generate deck"
	d.static_cards = data

	ResourceSaver.save(d, OUTPUT_PATH)

	return d
