package scala.meta.pc;

import java.util.Collections;
import java.util.List;
import org.eclipse.lsp4j.Range;

/** Experimental typed-invocation evidence, independent of Definition navigation. */
public final class ScalaConstructorEvidence {
  public static final String SCHEMA_VERSION = "metals-scala-constructor-evidence.v2";
  public final String schemaVersion = SCHEMA_VERSION;
  public final String uri;
  public final String sourceMd5;
  public final String status;
  public final String reason;
  public final List<Invocation> invocations;

  public ScalaConstructorEvidence(
      String uri, String sourceMd5, String status, String reason, List<Invocation> invocations) {
    this.uri = uri;
    this.sourceMd5 = sourceMd5;
    this.status = status;
    this.reason = reason;
    this.invocations = invocations;
  }

  public static ScalaConstructorEvidence unavailable(String uri, String sourceMd5, String reason) {
    return new ScalaConstructorEvidence(
        uri, sourceMd5, "UNAVAILABLE", reason, Collections.emptyList());
  }

  public static final class Invocation {
    public final Range invocationRange;
    public final Range calleeRange;
    public final String status;
    public final String reason;
    public final String selectedSymbol;
    public final String declaringOwnerSymbol;
    public final String declaringOwnerFqn;
    public final List<List<Parameter>> parameterLists;
    public final TypeReference constructedType;
    public final AnonymousConstructor anonymousConstructor;

    public Invocation(
        Range invocationRange,
        Range calleeRange,
        String status,
        String reason,
        String selectedSymbol,
        String declaringOwnerSymbol,
        String declaringOwnerFqn,
        List<List<Parameter>> parameterLists,
        TypeReference constructedType) {
      this(invocationRange, calleeRange, status, reason, selectedSymbol, declaringOwnerSymbol,
          declaringOwnerFqn, parameterLists, constructedType, null);
    }

    public Invocation(
        Range invocationRange,
        Range calleeRange,
        String status,
        String reason,
        String selectedSymbol,
        String declaringOwnerSymbol,
        String declaringOwnerFqn,
        List<List<Parameter>> parameterLists,
        TypeReference constructedType,
        AnonymousConstructor anonymousConstructor) {
      this.invocationRange = invocationRange;
      this.calleeRange = calleeRange;
      this.status = status;
      this.reason = reason;
      this.selectedSymbol = selectedSymbol;
      this.declaringOwnerSymbol = declaringOwnerSymbol;
      this.declaringOwnerFqn = declaringOwnerFqn;
      this.parameterLists = parameterLists;
      this.constructedType = constructedType;
      this.anonymousConstructor = anonymousConstructor;
    }
  }

  /**
   * A typed allocation of a source-backed anonymous class and its own generated initializer.
   * Compiler-local symbols are diagnostic identifiers, never stable cross-session identities.
   * Parameter counts describe the typed allocation before lambda lifting, not source parent
   * arguments or a JVM ABI (which may acquire captured/outer parameters later).
   */
  public static final class AnonymousConstructor {
    public final String initializerKind = "ANONYMOUS_CLASS_INITIALIZATION";
    public final String parameterSemantics = "TYPED_ALLOCATION_BEFORE_LAMBDA_LIFT";
    public final Range ownerRange;
    public final String ownerSymbol;
    public final String constructorSymbol;
    public final List<String> parentSymbols;
    public final int typedParameterCount;
    public final int typedArgumentCount;

    public AnonymousConstructor(
        Range ownerRange,
        String ownerSymbol,
        String constructorSymbol,
        List<String> parentSymbols,
        int typedParameterCount,
        int typedArgumentCount) {
      this.ownerRange = ownerRange;
      this.ownerSymbol = ownerSymbol;
      this.constructorSymbol = constructorSymbol;
      this.parentSymbols = parentSymbols;
      this.typedParameterCount = typedParameterCount;
      this.typedArgumentCount = typedArgumentCount;
    }
  }

  public static final class Parameter {
    public final String symbol;
    public final String name;
    public final TypeReference tpe;

    public Parameter(String symbol, String name, TypeReference tpe) {
      this.symbol = symbol;
      this.name = name;
      this.tpe = tpe;
    }
  }

  public static final class TypeReference {
    public final String kind = "type_ref";
    public final String symbol;
    public final List<TypeReference> arguments;

    public TypeReference(String symbol, List<TypeReference> arguments) {
      this.symbol = symbol;
      this.arguments = arguments;
    }
  }
}
