package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.Language;
import static org.testng.AssertJUnit.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

import java.io.IOException;

import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

public class ExtnSTResolverTest {

	private ExtnSTResolver r;

	@BeforeTest()
	public void setup() {
		this.r = new ExtnSTResolver();
	}

	@Test
	public void canResolvetest() {
		assertFalse(r.canResolve(Language.RUBY));
		assertTrue(r.canResolve(Language.SMALLTALK));
	}

	@Test
	public void nullByDefaultTest() throws IOException {
		assertEquals(null, r.resolve(new SourceFile("foo.st", "")));
	}

	@Test
	public void smalltalkExamplesTest() throws IOException {
		assertEquals(Language.SMALLTALK, r.resolve(new SourceFile("foo.st",
				"result := a > b\n" +
				"\tifTrue:[ 'greater' ]\n" +
				"\tifFalse:[ 'less or equal' ]\n" )));
		assertEquals(Language.SMALLTALK, r.resolve(new SourceFile("foo.st",
				"| aString vowels |\n" +
				"aString := 'This is a string'.\n" +
				"vowels := aString select: [:aCharacter | aCharacter isVowel].\n" )));
	}

	@Test
	public void notSmalltalkExamplesTest() throws IOException {
		assertEquals(null, r.resolve(new SourceFile("foo.st", "foo")));
		assertEquals(null, r.resolve(new SourceFile("foo.st", "")));
	}
}