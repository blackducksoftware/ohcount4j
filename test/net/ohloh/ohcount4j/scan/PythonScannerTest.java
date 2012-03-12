package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.LANG_PYTHON;

public class PythonScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, BLANK),   "\n");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, BLANK),   "     \n");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, BLANK),   "\t\n");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, CODE),    "value = string.capitalize(word)\n");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, COMMENT), "/* Block Comment */\n");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, COMMENT), "// Line comment\n");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, COMMENT), "# Line comment\n");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, COMMENT), "//\n");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, COMMENT), "#\n");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, CODE),    "value = string.capitalize(word) // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, BLANK),   "     ");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, BLANK),   "\t");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, CODE),    "value = string.capitalize(word)");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, COMMENT), "/* Block Comment */");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, COMMENT), "// Line comment");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, COMMENT), "# Line comment");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, COMMENT), "//");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, COMMENT), "#");
		assertLine(new PythonScanner(), new Line(LANG_PYTHON, CODE),    "value = string.capitalize(word) // with comment");
	}

	@Test
	public void helloWorld() {
		String code
			= "\"\"\"\n"
			+ "Assuming this is file mymodule.py, then this string, being the\n"
			+ "first statement in the file, will become the \"mymodule\" module's\n"
			+ "docstring when the file is imported.\n"
			+ "\"\"\"\n"
			+ "\t\n"
			+ "\"\"Not quite a docstring\"\"\n"
			+ "# Hello World\n"
			+ "// Written in Python\n"
			+ "print \"Hello World!\";\n";

		Line[] expected = {
			new Line(LANG_PYTHON, COMMENT),
			new Line(LANG_PYTHON, COMMENT),
			new Line(LANG_PYTHON, COMMENT),
			new Line(LANG_PYTHON, COMMENT),
			new Line(LANG_PYTHON, COMMENT),
			new Line(LANG_PYTHON, BLANK),
			new Line(LANG_PYTHON, CODE),
			new Line(LANG_PYTHON, COMMENT),
			new Line(LANG_PYTHON, COMMENT),
			new Line(LANG_PYTHON, CODE)
		};
		assertLines(new PythonScanner(), expected, code);
	}

	@Test
	public void unterminatedDocStringCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "'''\n\n\n";

		Line[] expected = {
				new Line(LANG_PYTHON, COMMENT),
				new Line(LANG_PYTHON, BLANK),
				new Line(LANG_PYTHON, BLANK)
			};
		assertLines(new PythonScanner(), expected, code);
	}
	
}