package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class CStyleScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.C, new Line(Language.C, BLANK),   "\n");
		assertLine(Language.C, new Line(Language.C, BLANK),   "     \n");
		assertLine(Language.C, new Line(Language.C, BLANK),   "\t\n");
		assertLine(Language.C, new Line(Language.C, CODE),    "#include <stdio.h>\n");
		assertLine(Language.C, new Line(Language.C, COMMENT), "/* Block Comment */\n");
		assertLine(Language.C, new Line(Language.C, COMMENT), "// Line comment\n");
		assertLine(Language.C, new Line(Language.C, COMMENT), "//\n");
		assertLine(Language.C, new Line(Language.C, CODE),    "#include <stdio.h> // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.C, new Line(Language.C, BLANK),   "     ");
		assertLine(Language.C, new Line(Language.C, BLANK),   "\t");
		assertLine(Language.C, new Line(Language.C, CODE),    "#include <stdio.h>");
		assertLine(Language.C, new Line(Language.C, COMMENT), "/* Block Comment */");
		assertLine(Language.C, new Line(Language.C, COMMENT), "// Line comment");
		assertLine(Language.C, new Line(Language.C, COMMENT), "//");
		assertLine(Language.C, new Line(Language.C, CODE),    "#include <stdio.h> // with comment");
	}

	@Test
	public void helloWorld() {
		String code
			= "/* Hello World\n"
			+ " * with multi-line comment */\n"
			+ "\n"
			+ "#include <stdio.h>\n"
			+ "\n"
			+ "main() {\n"
			+ "  printf(\"Hello world!\");\n"
			+ "}";

		Line[] expected = {
			new Line(Language.C, COMMENT),
			new Line(Language.C, COMMENT),
			new Line(Language.C, BLANK),
			new Line(Language.C, CODE),
			new Line(Language.C, BLANK),
			new Line(Language.C, CODE),
			new Line(Language.C, CODE),
			new Line(Language.C, CODE)
		};
		assertLines(Language.C, expected, code);
	}

	@Test
	public void unterminatedMultilineStringCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "'\nA\n\n";

		Line[] expected = {
				new Line(Language.C, CODE),
				new Line(Language.C, CODE),
				new Line(Language.C, BLANK)
			};
		assertLines(Language.C, expected, code);
	}

	@Test
	public void unterminatedBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "/*\n\n\n";

		Line[] expected = {
				new Line(Language.C, COMMENT),
				new Line(Language.C, BLANK),
				new Line(Language.C, BLANK)
			};
		assertLines(Language.C, expected, code);
	}
}
