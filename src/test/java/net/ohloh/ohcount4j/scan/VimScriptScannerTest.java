package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class VimScriptScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, BLANK),   "\n");
		assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, BLANK),   "     \n");
		assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, BLANK),   "\t\n");
		assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, CODE),    "if exists(\"g:syntax_on\")\n");
		assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, COMMENT), "\" Line comment\n");
		assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, COMMENT), "\"\n");
		assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, CODE),    "if exists(\"g:syntax_on\") \" with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, BLANK),   "     ");
		assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, BLANK),   "\t");
		assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, CODE),    "if exists(\"g:syntax_on\")");
		assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, COMMENT), "\" Line comment");
		assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, COMMENT), "\"");
		assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, CODE),    "if exists(\"g:syntax_on\") \" with comment");
	}

	@Test
	public void sampleTest() {
		String code
			= "\" Sample Script Written in Vimscript\n"
			+ "let i = 1\n"
			+ "while i < 5\n"
			+ "  echo \"count is\" i\n"
			+ "  let i += 1\n"
			+ "endwhile\n";

		Line[] expected = {
			new Line(Language.VIMSCRIPT, COMMENT),
			new Line(Language.VIMSCRIPT, CODE),
			new Line(Language.VIMSCRIPT, CODE),
			new Line(Language.VIMSCRIPT, CODE),
			new Line(Language.VIMSCRIPT, CODE),
			new Line(Language.VIMSCRIPT, CODE)
		};
		assertLines(Language.VIMSCRIPT, expected, code);
	}

}
