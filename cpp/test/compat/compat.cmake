function (run)
    execute_process (
        COMMAND ${ARGV}
        RESULT_VARIABLE error)
    if (error)
        message (FATAL_ERROR)
    endif()
endfunction()

if (${TEST} STREQUAL schema)
    run (${GBC} schema --runtime-schema ${COMPAT_DATA}/../schemas/compat.bond)
    run (${BOND_COMPAT} ${TEST} -d compat.Compat.json expected.gbc.${TEST} deserialized.gbc.${TEST})
endif()

if (CSHARP_COMPAT)
    run (${CSHARP_COMPAT} ${TEST} ${COMPAT_DATA}/compat.${TEST}.dat compat.${TEST}.cs.dat ${TEST})
    run (${BOND_COMPAT} ${TEST} -d compat.${TEST}.cs.dat expected.cs.${TEST} deserialized.cs.${TEST})

    if (${TEST} STREQUAL schema)
        run (${CSHARP_COMPAT} ${TEST} compat.Compat.json compat.${TEST}.gbc.dat)
    endif()
endif()
