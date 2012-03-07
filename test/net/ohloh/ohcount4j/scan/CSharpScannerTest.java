package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.*;

public class CSharpScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, BLANK),   "\n");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, BLANK),   "     \n");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, BLANK),   "\t\n");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, CODE),    "using System;\n");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, COMMENT), "/* Block Comment */\n");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, COMMENT), "// Line comment\n");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, CODE),    "Console.WriteLine(\"Hello, World!\"); // Single line comment on code line\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, BLANK),   "     ");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, BLANK),   "\t");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, CODE),    "using System;");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, COMMENT), "/* Block Comment */");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, COMMENT), "// Line comment");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, CODE),    "Console.WriteLine(\"Hello, World!\"); // Single line comment on code line");
	}

	@Test
	public void helloWorld() {
		String code
			= "/*\n"
			+ "	* HelloWorld\n"
			+ "\t\n"
			+ " * Simple C# program for testing purposes\n"
			+ " */\n"
			+ "using System;\n"
			+ "\n"
			+ "public class HelloWorld {\n"
			+ "		// Single line comment\n"
			+ "		public static void Main() {\n"
			+ "			Console.WriteLine(\"Hello, World!\"); // Single line comment on code line\n"
			+ "		}\n"
			+ "}\n";

		Line[] expected = {
				new Line(LANG_CSHARP, COMMENT),
				new Line(LANG_CSHARP, COMMENT),
				new Line(LANG_CSHARP, BLANK),
				new Line(LANG_CSHARP, COMMENT),
				new Line(LANG_CSHARP, COMMENT),
				new Line(LANG_CSHARP, CODE),
				new Line(LANG_CSHARP, BLANK),
				new Line(LANG_CSHARP, CODE),
				new Line(LANG_CSHARP, COMMENT),
				new Line(LANG_CSHARP, CODE),
				new Line(LANG_CSHARP, CODE),
				new Line(LANG_CSHARP, CODE),
				new Line(LANG_CSHARP, CODE)
		};
		assertLines(new CSharpScanner(), expected, code);
	}
	
}