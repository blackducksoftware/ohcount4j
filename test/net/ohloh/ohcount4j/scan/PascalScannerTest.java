package net.ohloh.ohcount4j.scan;

import static net.ohloh.ohcount4j.Entity.BLANK;
import static net.ohloh.ohcount4j.Entity.CODE;
import static net.ohloh.ohcount4j.Entity.COMMENT;
import static net.ohloh.ohcount4j.Language.LANG_PASCAL;

import org.testng.annotations.Test;

public class PascalScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, BLANK),   "\n");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, BLANK),   "     \n");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, BLANK),   "\t\n");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, CODE),    "Infile_name := 'A:\' + 'PROG006' + '.OUT';\n");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, COMMENT), "{ Bracket Comment }\n");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, COMMENT), "(* Starparen Comment *)\n");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, COMMENT), "{ (* Nested Comment *) }\n");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, COMMENT), "(* { Nested Comment } *)\n");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, COMMENT), "// Line comment\n");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, CODE),    "File_name = String[14]; { with comment }\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, BLANK),   "     ");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, BLANK),   "\t");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, CODE),    "Infile_name := 'A:\' + 'PROG006' + '.OUT';");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, COMMENT), "{ Bracket Comment }");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, COMMENT), "(* Starparen Comment *)");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, COMMENT), "{ (* Nested Comment *) }");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, COMMENT), "(* { Nested Comment } *)");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, COMMENT), "// Line comment");
		assertLine(new PascalScanner(), new Line(LANG_PASCAL, CODE),    "File_name = String[14]; { with comment }");
	}

	@Test
	public void simpleTest() {
		String code
			= "{\n"
			+ "multi-line comment\n"
			+ "\n"
			+ "}\n;"
			+ "File_name = String[14]; (* with multi\n"
			+ "\t\n"
			+ "line comment *)\n"
			+ "{ comment (* nested comment *) more comment }";

		Line[] expected = {
			new Line(LANG_PASCAL, COMMENT),
			new Line(LANG_PASCAL, COMMENT),
			new Line(LANG_PASCAL, BLANK),
			new Line(LANG_PASCAL, COMMENT),
			new Line(LANG_PASCAL, CODE),
			new Line(LANG_PASCAL, BLANK),
			new Line(LANG_PASCAL, COMMENT),
			new Line(LANG_PASCAL, COMMENT)
		};
		assertLines(new PascalScanner(), expected, code);
	}

	@Test
	public void unterminatedBracketBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "{\n\n\n";

		Line[] expected = {
				new Line(LANG_PASCAL, COMMENT),
				new Line(LANG_PASCAL, BLANK),
				new Line(LANG_PASCAL, BLANK)
			};
		assertLines(new PascalScanner(), expected, code);
	}
	
	@Test
	public void unterminatedStarparenBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "(*\n\n\n";

		Line[] expected = {
				new Line(LANG_PASCAL, COMMENT),
				new Line(LANG_PASCAL, BLANK),
				new Line(LANG_PASCAL, BLANK)
			};
		assertLines(new PascalScanner(), expected, code);
	}
	
	@Test
	public void unterminatedNestedBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "{\n(*\n\n\n*)\n";

		Line[] expected = {
				new Line(LANG_PASCAL, COMMENT),
				new Line(LANG_PASCAL, COMMENT),
				new Line(LANG_PASCAL, BLANK),
				new Line(LANG_PASCAL, BLANK),
				new Line(LANG_PASCAL, COMMENT)
			};
		assertLines(new PascalScanner(), expected, code);
	}
	
}
