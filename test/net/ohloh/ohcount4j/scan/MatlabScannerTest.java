package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class MatlabScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, BLANK),   "\n");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, BLANK),   "     \n");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, BLANK),   "\t\n");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, CODE),    "Horiz = [1,2,3];\n");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, COMMENT), "%{ Block comment }%\n");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, COMMENT), "% Line comment\n");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, COMMENT), "...Line comment\n");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, COMMENT), "%\n");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, COMMENT), "...\n");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, CODE),    "f = inline('2*x*y', 'x', 'y'); % with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, BLANK),   "     ");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, BLANK),   "\t");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, CODE),    "Horiz = [1,2,3];");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, COMMENT), "%{ Block comment }%");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, COMMENT), "% Line comment");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, COMMENT), "...Line comment");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, COMMENT), "%");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, COMMENT), "...");
		assertLine(new MatlabScanner(), new Line(Language.MATLAB, CODE),    "f = inline('2*x*y', 'x', 'y'); % with comment");
	}

	@Test
	public void sampleTest() {
		String code
			= "%{ Multi\n"
			+ "\t\n"
			+ "line comment %}\n"
			+ "for n= 3:length(Xw),\n"
			+ "	    y(n)=sum(Xw(n-2:n))/3;       %y[n] is the filtered signal\n"
			+ "end\n"
			+ "\n"
			+ "% Single Line Comment\n"
			+ "			...continued on next line with line continue function\n"
			+ "plot(y);\n"
			+ "hold;\n"
			+ "%";

		Line[] expected = {
			new Line(Language.MATLAB, COMMENT),
			new Line(Language.MATLAB, BLANK),
			new Line(Language.MATLAB, COMMENT),
			new Line(Language.MATLAB, CODE),
			new Line(Language.MATLAB, CODE),
			new Line(Language.MATLAB, CODE),
			new Line(Language.MATLAB, BLANK),
			new Line(Language.MATLAB, COMMENT),
			new Line(Language.MATLAB, COMMENT),
			new Line(Language.MATLAB, CODE),
			new Line(Language.MATLAB, CODE),
			new Line(Language.MATLAB, COMMENT)
		};
		assertLines(new MatlabScanner(), expected, code);
	}

	@Test
	public void unterminatedBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "%{\n\n\n";

		Line[] expected = {
				new Line(Language.MATLAB, COMMENT),
				new Line(Language.MATLAB, BLANK),
				new Line(Language.MATLAB, BLANK)
			};
		assertLines(new MatlabScanner(), expected, code);
	}
}
