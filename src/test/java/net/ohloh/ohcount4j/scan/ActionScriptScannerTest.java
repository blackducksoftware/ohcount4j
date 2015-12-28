package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class ActionScriptScannerTest extends AbstractBaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, BLANK),   "\n");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, BLANK),   "     \n");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, BLANK),   "\t\n");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, CODE),    "var greeting:String = \"Hello World!\";\n");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, COMMENT), "/* Block Comment */\n");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, COMMENT), "// Line comment\n");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, COMMENT), "//\n");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, CODE),    "var greeting:String = \"Hello World!\"; // with comment\n");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, CODE),    "<![CDATA[ string containing anything \\ /* */ ]]\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, BLANK),   "     ");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, BLANK),   "\t");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, CODE),    "var greeting:String = \"Hello World!\";");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, COMMENT), "/* Block Comment */");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, COMMENT), "// Line comment");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, COMMENT), "//");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, CODE),    "var greeting:String = \"Hello World!\"; // with comment");
		assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, CODE),    "<![CDATA[ string containing anything \\ /* */ ]]");
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
			new Line(Language.ACTIONSCRIPT, COMMENT),
			new Line(Language.ACTIONSCRIPT, COMMENT),
			new Line(Language.ACTIONSCRIPT, BLANK),
			new Line(Language.ACTIONSCRIPT, COMMENT),
			new Line(Language.ACTIONSCRIPT, CODE),
			new Line(Language.ACTIONSCRIPT, CODE),
			new Line(Language.ACTIONSCRIPT, COMMENT),
			new Line(Language.ACTIONSCRIPT, CODE),
			new Line(Language.ACTIONSCRIPT, CODE),
			new Line(Language.ACTIONSCRIPT, CODE),
			new Line(Language.ACTIONSCRIPT, CODE),
			new Line(Language.ACTIONSCRIPT, CODE),
			new Line(Language.ACTIONSCRIPT, CODE),
			new Line(Language.ACTIONSCRIPT, CODE),
			new Line(Language.ACTIONSCRIPT, CODE)
		};
		assertLines(Language.ACTIONSCRIPT, expected, code);
	}

	@Test
	public void unterminatedCDataStringCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "<![CDATA[\nA\n\n";

		Line[] expected = {
				new Line(Language.ACTIONSCRIPT, CODE),
				new Line(Language.ACTIONSCRIPT, CODE),
				new Line(Language.ACTIONSCRIPT, BLANK)
			};
		assertLines(Language.ACTIONSCRIPT, expected, code);
	}

}
