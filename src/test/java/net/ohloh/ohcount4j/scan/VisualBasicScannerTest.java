package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class VisualBasicScannerTest extends AbstractBaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.VB, new Line(Language.VB, BLANK),   "\n");
		assertLine(Language.VB, new Line(Language.VB, BLANK),   "     \n");
		assertLine(Language.VB, new Line(Language.VB, BLANK),   "\t\n");
		assertLine(Language.VB, new Line(Language.VB, CODE),    "Private testVar As Integer\n");
		assertLine(Language.VB, new Line(Language.VB, COMMENT), "REM Line comment started with REM\n");
		assertLine(Language.VB, new Line(Language.VB, COMMENT), "' Line comment\n");
		assertLine(Language.VB, new Line(Language.VB, COMMENT), "'\n");
		assertLine(Language.VB, new Line(Language.VB, COMMENT), "REM\n");
		assertLine(Language.VB, new Line(Language.VB, CODE),    "Set(ByVal value As Integer) ' with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.VB, new Line(Language.VB, BLANK),   "     ");
		assertLine(Language.VB, new Line(Language.VB, BLANK),   "\t");
		assertLine(Language.VB, new Line(Language.VB, CODE),    "Private testVar As Integer");
		assertLine(Language.VB, new Line(Language.VB, COMMENT), "REM Line comment started");
		assertLine(Language.VB, new Line(Language.VB, COMMENT), "' Line comment");
		assertLine(Language.VB, new Line(Language.VB, COMMENT), "'");
		assertLine(Language.VB, new Line(Language.VB, COMMENT), "REM");
		assertLine(Language.VB, new Line(Language.VB, CODE),    "Set(ByVal value As Integer) ' with comment");
	}

	@Test
	public void helloWorld() {
		String code
			= "' Sample Test VB Program\n"
			+ "Class propClass\n"
			+ "		Private propVal As Integer\n"
			+ "	    Property prop1() As Integer ' Method to Set value\n"
			+ "			REM Some random line comment\n"
			+ "	        Set(ByVal value As Integer)\n"
			+ "	            propVal = value\n"
			+ "	        End Set\n"
			+ "	    End Property\n"
			+ "	End Class\n";
		

		Line[] expected = {
			new Line(Language.VB, COMMENT),
			new Line(Language.VB, CODE),
			new Line(Language.VB, CODE),
			new Line(Language.VB, CODE),
			new Line(Language.VB, COMMENT),
			new Line(Language.VB, CODE),
			new Line(Language.VB, CODE),
			new Line(Language.VB, CODE),
			new Line(Language.VB, CODE),
			new Line(Language.VB, CODE)
		};
		assertLines(Language.VB, expected, code);
	}

	@Test
	public void unterminatedMultilineStringCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "\"\nA\n\n";

		Line[] expected = {
				new Line(Language.VB, CODE),
				new Line(Language.VB, CODE),
				new Line(Language.VB, BLANK)
			};
		assertLines(Language.VB, expected, code);
	}

}
