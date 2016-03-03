/*
 * Copyright 2016 Black Duck Software, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.blackducksoftware.ohcount4j;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * Represents the diff per Language for a file change.
 *
 * @author mpujari
 *
 */
public class LanguageDiff {

    private final Language language;

    private final int codeLinesAdded;

    private final int codeLinesRemoved;

    private final int commentLinesAdded;

    private final int commentLinesRemoved;

    private final int blankLinesAdded;

    private final int blankLinesRemoved;

    public LanguageDiff(Language language, int codeLinesAdded, int codeLinesRemoved, int commentLinesAdded,
            int commentLinesRemoved, int blankLinesAdded, int blankLinesRemoved) {
        this.language = language;
        this.codeLinesAdded = codeLinesAdded;
        this.codeLinesRemoved = codeLinesRemoved;
        this.commentLinesAdded = commentLinesAdded;
        this.commentLinesRemoved = commentLinesRemoved;
        this.blankLinesAdded = blankLinesAdded;
        this.blankLinesRemoved = blankLinesRemoved;
    }

    public Language getLanguage() {
        return language;
    }

    public int getCodeLinesAdded() {
        return codeLinesAdded;
    }

    public int getCodeLinesRemoved() {
        return codeLinesRemoved;
    }

    public int getCommentLinesAdded() {
        return commentLinesAdded;
    }

    public int getCommentLinesRemoved() {
        return commentLinesRemoved;
    }

    public int getBlankLinesAdded() {
        return blankLinesAdded;
    }

    public int getBlankLinesRemoved() {
        return blankLinesRemoved;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof LanguageDiff)) {
            return false;
        }
        LanguageDiff t = (LanguageDiff) obj;
        return t.language == language && t.codeLinesAdded == codeLinesAdded && t.codeLinesRemoved == codeLinesRemoved
                && t.commentLinesAdded == commentLinesAdded && t.commentLinesRemoved == commentLinesRemoved
                && t.blankLinesAdded == blankLinesAdded && t.blankLinesRemoved == blankLinesRemoved;
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(language).append(codeLinesAdded).append(codeLinesRemoved)
                .append(commentLinesAdded).append(commentLinesRemoved)
                .append(blankLinesAdded).append(blankLinesRemoved).toHashCode();
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this)
        .append("Language", language)
        .append("CodeAdded", codeLinesAdded)
        .append("CodeRemoved", codeLinesRemoved)
        .append("CommentAdded", commentLinesAdded)
        .append("CommentRemoved", commentLinesRemoved)
        .append("BlanksAdded", blankLinesAdded)
        .append("BlanksRemoved", blankLinesRemoved).build();
    }
}
