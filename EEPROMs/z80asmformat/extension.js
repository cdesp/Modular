// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require('vscode');
var tabsize = 8;

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed

/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "z80asmformat" is now active!');

	// The command has been defined in the package.json file
	// Now provide the implementation of the command with  registerCommand
	// The commandId parameter must match the command field in package.json
	let disposable = vscode.commands.registerCommand('z80asmformat.format', function () {
		const editor = vscode.window.activeTextEditor;
		if (editor)
			tabsize = editor.options.tabSize;
		if (updateAsm()) {
			vscode.window.showInformationMessage('The assembly code has been formated');
		} else {
			vscode.window.showErrorMessage("You can only format .z80 or .s files");
		}
	});


	context.subscriptions.push(disposable);
}

// this method is called when your extension is deactivated
function deactivate() { }

module.exports = {
	activate,
	deactivate
}


function updateAsm() {
	const { activeTextEditor } = vscode.window;

	if (activeTextEditor) {
		var fileExt = activeTextEditor.document.fileName.split(".").pop();
		console.log(fileExt);
		if ((fileExt === "z80") || (fileExt === "s") || (fileExt === "Z80") || (fileExt === "S")) {
			const { document } = activeTextEditor;

			const content = format(document.getText());

			const firstLine = document.lineAt(0);
			const lastLine = document.lineAt(document.getText().split("\n").length - 1);
			const edit = new vscode.WorkspaceEdit();
			edit.delete(document.uri, new vscode.Range(firstLine.range.start, lastLine.range.end))
			edit.insert(document.uri, firstLine.range.start, content);

			vscode.workspace.applyEdit(edit);
			return true;
		} else {
			return false;
		}
	}
}

String.prototype.replaceAt = function (index, replacement) {
	return this.substr(0, index) + replacement + this.substr(index + 1);
}

function isLabel(linea) {
	var instr = false;
	for (var j = 0; j < linea.length; ++j) {
		if (linea[j] == '"') instr = !instr;
		if (instr) continue;
		if (linea[j] == ';') return false;
		if (linea[j] == ':' && linea[j + 1] != "'") return true;
	}
	return false;
}

function getPos(line, match) {
	var lines = line.split("")
	var others = false;
	for (var j = 0; j < lines.length; ++j) {
		if (lines[j] == match) {
			if (others) return j;
			else return -1;
		}
		else if (lines[j] != ' ' &&
			lines[j] != '\t' &&
			lines[j] != '\r' &&
			lines[j] != '\n' &&
			lines[j] != '\v') {
			others = true;
		}
	}
	return -1;
}

function z80cmdsnotreplace(teststr) { //check for commands not to be replaced
	var cmds = ["DB ", "db ", "DEFB ", "defb "];


	for (var i = 0; i < cmds.length; i++) {
		if (teststr.indexOf(cmds[i]) > -1) {
			return true;
		}
	}
	return false;
}

function format(text) {
	var code = text.trim();
	var lines = code.split("\n");

	//Align left lines
	for (var i = 0; i < lines.length; ++i) {
		lines[i] = lines[i].trim();
		var posComment = getPos(lines[i], ';');
		if (lines[i].indexOf(";") > -1 && posComment == -1) continue;
		var posString = getPos(lines[i], '"');
		var comnt = ""; var lnstr = "";
		if (posComment > 0) {//do not replace in comments
			if (posString > 0 && posString < posComment) { //do not replace in comments
				var tmps1 = lines[i].substr(0, posString);
				var tmps2 = lines[i].substr(posString, 100);
				var secstr = getPos(tmps2, '"');
				var comstr = getPos(tmps2, ';');
				if (secstr > 0 && secstr < comstr) { //comma not in text
					comnt = lines[i].substr(posComment, 100);
					lines[i] = lines[i].substr(0, posComment - 1);
				}
			} else {
				comnt = lines[i].substr(posComment, 100);
				lines[i] = lines[i].substr(0, posComment - 1);
			}
		}

		if (posString > 0 && posString < posComment) { //do not replace in comments
			comnt = lines[i].substr(posComment, 100);
			lines[i] = lines[i].substr(0, posComment - 1);
		}
		posString = getPos(lines[i], '"');
		if (posString > 0 && lines[i][posString - 1] != "'") { //do not replace inside strings
			lnstr = ' ' + lines[i].substr(posString, 100);
			lines[i] = lines[i].substr(0, posString - 1);
		}
		if (!z80cmdsnotreplace(lines[i])) {
			lines[i] = lines[i].replace(/(\s*),(\s*)/g, ", ");
			lines[i] = lines[i].replace(/(\s*)\+(\s*)/g, ' + ');
			lines[i] = lines[i].replace(/(\s*)\-(\s*)/g, ' - ');
			lines[i] = lines[i].replace(/(\s*)\*(\s*)/g, ' * ');
			lines[i] = lines[i].replace(/(\s*)\/(\s*)/g, ' / ');
			lines[i] = lines[i].replace(/ +(?= )/g, '');
		}
		lines[i] = lines[i].replace(/(\s*)\t(\s*)/g, ' ');  //remove tabs always
		lines[i] = lines[i] + lnstr + comnt;
		var islbl = isLabel(lines[i]);
		var tabspces = 0;
		var lblend = 0;
		if (!islbl) {
			lines[i] = "\t\t" + lines[i];
			tabspces = 2;
		}
		else {
			lblend = lines[i].indexOf(":");
			if (lines[i][lblend + 1] == ' ') {
				lines[i] = lines[i].replaceAt(lblend + 1, "");
			}
			if (lblend < tabsize - 1) {
				lines[i] = lines[i].replaceAt(lblend, ":\t\t");
				tabspces = 2;
			}
			else {
				lines[i] = lines[i].replaceAt(lblend, ":\t");
				tabspces = 1;
			}
		}
		posComment = getPos(lines[i], ';');
		if (posComment != -1) {
			var spaces = ""
			if (islbl) {
				spaces = "";
				lblend++;
			}
			var cpos = Math.floor((posComment - tabspces - lblend) / tabsize);
			var needtabs = tabsize - cpos;
			for (var k = 0; k < needtabs; k++) {
				spaces += "\t";
			}

			//for (var k = posComment - tabspces; k < 45; ++k) {
			//	spaces += " "
			//}
			//lines[i] = lines[i].replaceAt(posComment, spaces + ';');
			lines[i] = lines[i].replaceAt(posComment, spaces + ';');
		}
	}

	//Reset Code
	code = "";
	for (i = 0; i < lines.length; ++i) {
		code += lines[i] + "\n";
	}

	return code;
}