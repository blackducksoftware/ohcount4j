package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class FortranFreeScannerTest extends AbstractBaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, BLANK),   "\n");
		assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, BLANK),   "     \n");
		assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, BLANK),   "\t\n");
		assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, CODE),    "Print *, \"Hello World!\"\n");
		assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, COMMENT), "! Line comment\n");
		assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, COMMENT), "!\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, BLANK),   "     ");
		assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, BLANK),   "\t");
		assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, CODE),    "Print *, \"Hello World!\"");
		assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, COMMENT), "! Line comment");
		assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, COMMENT), "!");
	}

	@Test
	public void sampleTest() {
		String code
				= "! Simple Hello World Program\n"
				+ "! Written in Fortran Free Format\n"
				+ "Program Hello\n"
				+ "   Print *, \"Hello World!\"\n"
				+ "End Program Hello\n";

		Line[] expected = {
			new Line(Language.FORTRANFREE, COMMENT),
			new Line(Language.FORTRANFREE, COMMENT),
			new Line(Language.FORTRANFREE, CODE),
			new Line(Language.FORTRANFREE, CODE),
			new Line(Language.FORTRANFREE, CODE)
		};
		assertLines(Language.FORTRANFREE, expected, code);
	}
	
}
