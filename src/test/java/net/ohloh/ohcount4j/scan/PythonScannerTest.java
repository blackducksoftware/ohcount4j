package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class PythonScannerTest extends AbstractBaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.PYTHON, new Line(Language.PYTHON, BLANK),   "\n");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, BLANK),   "     \n");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, BLANK),   "\t\n");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, CODE),    "value = string.capitalize(word)\n");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "/* Block Comment */\n");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "// Line comment\n");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "# Line comment\n");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "//\n");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "#\n");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, CODE),    "value = string.capitalize(word) // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.PYTHON, new Line(Language.PYTHON, BLANK),   "     ");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, BLANK),   "\t");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, CODE),    "value = string.capitalize(word)");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "/* Block Comment */");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "// Line comment");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "# Line comment");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "//");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "#");
		assertLine(Language.PYTHON, new Line(Language.PYTHON, CODE),    "value = string.capitalize(word) // with comment");
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
			new Line(Language.PYTHON, COMMENT),
			new Line(Language.PYTHON, COMMENT),
			new Line(Language.PYTHON, COMMENT),
			new Line(Language.PYTHON, COMMENT),
			new Line(Language.PYTHON, COMMENT),
			new Line(Language.PYTHON, BLANK),
			new Line(Language.PYTHON, CODE),
			new Line(Language.PYTHON, COMMENT),
			new Line(Language.PYTHON, COMMENT),
			new Line(Language.PYTHON, CODE)
		};
		assertLines(Language.PYTHON, expected, code);
	}

	@Test
	public void unterminatedDocStringCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "'''\n\n\n";

		Line[] expected = {
				new Line(Language.PYTHON, COMMENT),
				new Line(Language.PYTHON, BLANK),
				new Line(Language.PYTHON, BLANK)
			};
		assertLines(Language.PYTHON, expected, code);
	}
	
}
