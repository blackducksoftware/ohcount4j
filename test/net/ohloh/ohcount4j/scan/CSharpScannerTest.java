package net.ohloh.ohcount4j.scan;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.*;

public class CSharpScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, BLANK),   "\n");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, BLANK),   "     \n");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, BLANK),   "\t\n");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, CODE),    "#include <stdio.h>\n");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, COMMENT), "/* Block Comment */\n");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, COMMENT), "// Line comment\n");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, CODE),    "#include <stdio.h> // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, BLANK),   "     ");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, BLANK),   "\t");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, CODE),    "#include <stdio.h>");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, COMMENT), "/* Block Comment */");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, COMMENT), "// Line comment");
		assertLine(new CSharpScanner(), new Line(LANG_CSHARP, CODE),    "#include <stdio.h> // with comment");
	}

	@Test
	public void helloWorld() {
		String code
			= "/* Hello World\n"
			+ "\t\n"
			+ " * with multi-line comment */\n"
			+ "\n"
			+ "#include <stdio.h>\n"
			+ "\n"
			+ "main() {\n"
			+ "  printf(\"Hello world!\");\n"
			+ "}";

		Line[] expected = {
			new Line(LANG_CSHARP, COMMENT),
			new Line(LANG_CSHARP, BLANK),
			new Line(LANG_CSHARP, COMMENT),
			new Line(LANG_CSHARP, BLANK),
			new Line(LANG_CSHARP, CODE),
			new Line(LANG_CSHARP, BLANK),
			new Line(LANG_CSHARP, CODE),
			new Line(LANG_CSHARP, CODE),
			new Line(LANG_CSHARP, CODE)
		};
		assertLines(new CSharpScanner(), expected, code);
	}
	
	@Test
	public void testFile() {
		String s = readFile("test/data/sample.cs");
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
		assertLines(new CSharpScanner(), expected, s);
	}
	
	public String readFile(String loc) {
		try {
			String s = "";
			BufferedReader reader = new BufferedReader(new FileReader(loc));
			String line = reader.readLine();
			while (line != null) {
				s += line + "\n";
				line = reader.readLine();
			}
			return s;
		}
		catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}
	
}