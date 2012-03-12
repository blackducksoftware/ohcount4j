package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.LANG_ACTIONSCRIPT;

public class ActionScriptScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, BLANK),   "\n");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, BLANK),   "     \n");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, BLANK),   "\t\n");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, CODE),    "var greeting:String = \"Hello World!\";\n");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, COMMENT), "/* Block Comment */\n");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, COMMENT), "// Line comment\n");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, COMMENT), "//\n");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, CODE),    "var greeting:String = \"Hello World!\"; // with comment\n");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, CODE),    "<![CDATA[ string containing anything \\ /* */ ]]\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, BLANK),   "     ");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, BLANK),   "\t");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, CODE),    "var greeting:String = \"Hello World!\";");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, COMMENT), "/* Block Comment */");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, COMMENT), "// Line comment");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, COMMENT), "//");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, CODE),    "var greeting:String = \"Hello World!\"; // with comment");
		assertLine(new ActionScriptScanner(), new Line(LANG_ACTIONSCRIPT, CODE),    "<![CDATA[ string containing anything \\ /* */ ]]");
	}

	@Test
	public void helloWorld() {
		String code
			= "/* Hello World\n"
			+ "		in ActionScript\n"
			+ "\t\n"
			+ "*/\n"
			+ "package ca.flashdev.hello {\n"
			+ "		public class HelloWorld {\n"
			+ "			// Print's Hello World\n"
			+ "			public function sayHello():String {\n"
			+ "				var greeting:String = <![CDATA[ \n"
			+ "										/* Hello World! */\n"
			+ "										]];\n"
			+ "				return greeting;\n"
    		+ "			}\n"
			+ "		}\n"
			+ "}\n";

		Line[] expected = {
			new Line(LANG_ACTIONSCRIPT, COMMENT),
			new Line(LANG_ACTIONSCRIPT, COMMENT),
			new Line(LANG_ACTIONSCRIPT, BLANK),
			new Line(LANG_ACTIONSCRIPT, COMMENT),
			new Line(LANG_ACTIONSCRIPT, CODE),
			new Line(LANG_ACTIONSCRIPT, CODE),
			new Line(LANG_ACTIONSCRIPT, COMMENT),
			new Line(LANG_ACTIONSCRIPT, CODE),
			new Line(LANG_ACTIONSCRIPT, CODE),
			new Line(LANG_ACTIONSCRIPT, CODE),
			new Line(LANG_ACTIONSCRIPT, CODE),
			new Line(LANG_ACTIONSCRIPT, CODE),
			new Line(LANG_ACTIONSCRIPT, CODE),
			new Line(LANG_ACTIONSCRIPT, CODE),
			new Line(LANG_ACTIONSCRIPT, CODE)
		};
		assertLines(new ActionScriptScanner(), expected, code);
	}

	@Test
	public void unterminatedCDataStringCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "<![CDATA[\nA\n\n";

		Line[] expected = {
				new Line(LANG_ACTIONSCRIPT, CODE),
				new Line(LANG_ACTIONSCRIPT, CODE),
				new Line(LANG_ACTIONSCRIPT, BLANK)
			};
		assertLines(new ActionScriptScanner(), expected, code);
	}

}