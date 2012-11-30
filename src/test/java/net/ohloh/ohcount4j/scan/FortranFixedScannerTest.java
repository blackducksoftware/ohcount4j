package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class FortranFixedScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, BLANK),   "\n");
		assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, BLANK),   "     \n");
		assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, CODE),    "       print *, \"Hello World!\"\n");
		assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, COMMENT), "C Line comment\n");
		assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, COMMENT), "C\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, BLANK),   "     ");
		assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, CODE),    "       print *, \"Hello World!\"");
		assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, COMMENT), "C Line comment");
		assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, COMMENT), "C");
	}

	@Test
	public void sampleTest() {
		String code
			= "C Simple Hello World Program\n"
			+ "C Written in Fortran Fixed Format\n"
			+ "       program hello\n"
			+ "          print *, \"Hello World!\"\n"
			+ "	      end program hello\n";

		Line[] expected = {
			new Line(Language.FORTRANFIXED, COMMENT),
			new Line(Language.FORTRANFIXED, COMMENT),
			new Line(Language.FORTRANFIXED, CODE),
			new Line(Language.FORTRANFIXED, CODE),
			new Line(Language.FORTRANFIXED, CODE)
		};
		assertLines(Language.FORTRANFIXED, expected, code);
	}
	
}
