package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class CSharpScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new CSharpScanner(), new Line(Language.CSHARP, BLANK),   "\n");
		assertLine(new CSharpScanner(), new Line(Language.CSHARP, BLANK),   "     \n");
		assertLine(new CSharpScanner(), new Line(Language.CSHARP, BLANK),   "\t\n");
		assertLine(new CSharpScanner(), new Line(Language.CSHARP, CODE),    "using System;\n");
		assertLine(new CSharpScanner(), new Line(Language.CSHARP, COMMENT), "/* Block Comment */\n");
		assertLine(new CSharpScanner(), new Line(Language.CSHARP, COMMENT), "// Line comment\n");
		assertLine(new CSharpScanner(), new Line(Language.CSHARP, CODE),    "Console.WriteLine(\"Hello, World!\"); // Single line comment on code line\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new CSharpScanner(), new Line(Language.CSHARP, BLANK),   "     ");
		assertLine(new CSharpScanner(), new Line(Language.CSHARP, BLANK),   "\t");
		assertLine(new CSharpScanner(), new Line(Language.CSHARP, CODE),    "using System;");
		assertLine(new CSharpScanner(), new Line(Language.CSHARP, COMMENT), "/* Block Comment */");
		assertLine(new CSharpScanner(), new Line(Language.CSHARP, COMMENT), "// Line comment");
		assertLine(new CSharpScanner(), new Line(Language.CSHARP, CODE),    "Console.WriteLine(\"Hello, World!\"); // Single line comment on code line");
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
				new Line(Language.CSHARP, COMMENT),
				new Line(Language.CSHARP, COMMENT),
				new Line(Language.CSHARP, BLANK),
				new Line(Language.CSHARP, COMMENT),
				new Line(Language.CSHARP, COMMENT),
				new Line(Language.CSHARP, CODE),
				new Line(Language.CSHARP, BLANK),
				new Line(Language.CSHARP, CODE),
				new Line(Language.CSHARP, COMMENT),
				new Line(Language.CSHARP, CODE),
				new Line(Language.CSHARP, CODE),
				new Line(Language.CSHARP, CODE),
				new Line(Language.CSHARP, CODE)
		};
		assertLines(new CSharpScanner(), expected, code);
	}
	
}
