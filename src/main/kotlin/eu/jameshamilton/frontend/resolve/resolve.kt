package eu.jameshamilton.frontend.resolve

import eu.jameshamilton.frontend.Program

fun resolve(program: Program): Program {
    var resolved = resolveVariables(program)
    resolved = resolveLabels(resolved)
    return resolved
}
