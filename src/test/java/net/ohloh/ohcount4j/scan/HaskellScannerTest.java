package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class HaskellScannerTest extends AbstractBaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.HASKELL, new Line(Language.HASKELL, BLANK),   "\n");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, BLANK),   "     \n");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, BLANK),   "\t\n");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, CODE),    "main =  print [ (n, product [1..n]) | n <- [1..20]]\n");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, COMMENT), "-- Line comment\n");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, COMMENT), "{- Block comment -}\n");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, COMMENT), "--\n");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, CODE), "|--\n");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, CODE), "-->\n");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, CODE),    "main =  print [ (n, product [1..n]) | n <- [1..20]] -- with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.HASKELL, new Line(Language.HASKELL, BLANK),   "     ");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, BLANK),   "\t");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, CODE),    "main =  print [ (n, product [1..n]) | n <- [1..20]]");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, COMMENT), "-- Line comment");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, COMMENT), "{- Block comment -}");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, COMMENT), "--");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, CODE), "|--");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, CODE), "-->");
		assertLine(Language.HASKELL, new Line(Language.HASKELL, CODE),    "main =  print [ (n, product [1..n]) | n <- [1..20]] -- with comment");
	}

	@Test
	public void sampleTest() {
		String code
			= "{- Simple Haskell Program\n"
			+ "			{- For Testing Purposes\n"
			+ "\t\n"
			+ "			-}\n"
			+ "-}\n"
			+ "module Main where\n"
			+ "\t\n"
			+ "main :: IO ()\n"
			+ "-- Prints \"Hello, World!\" to the screen\n"
			+ "main = putStrLn \"Hello, World!\"\n"
			+ "--> This is not a valid comment\n"
			+ "|-- This is also not a valid comment\n"
			+ "--";

		Line[] expected = {
			new Line(Language.HASKELL, COMMENT),
			new Line(Language.HASKELL, COMMENT),
			new Line(Language.HASKELL, BLANK),
			new Line(Language.HASKELL, COMMENT),
			new Line(Language.HASKELL, COMMENT),
			new Line(Language.HASKELL, CODE),
			new Line(Language.HASKELL, BLANK),
			new Line(Language.HASKELL, CODE),
			new Line(Language.HASKELL, COMMENT),
			new Line(Language.HASKELL, CODE),
			new Line(Language.HASKELL, CODE),
			new Line(Language.HASKELL, CODE),
			new Line(Language.HASKELL, COMMENT)
		};
		assertLines(Language.HASKELL, expected, code);
	}
	
	@Test
	public void unterminatedNestedBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "{-\n {-\n -}\n\n\n";

		Line[] expected = {
				new Line(Language.HASKELL, COMMENT),
				new Line(Language.HASKELL, COMMENT),
				new Line(Language.HASKELL, COMMENT),
				new Line(Language.HASKELL, BLANK),
				new Line(Language.HASKELL, BLANK)
			};
		assertLines(Language.HASKELL, expected, code);
	}
}
