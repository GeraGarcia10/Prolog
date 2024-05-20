:- use_module(library(pce)).
:- use_module(library(pce_style_item)).

% Base de conocimientos 
% Plantas medicinales y sus propiedades
planta(manzanilla).
planta(menta).
planta(eucalipto).
planta(tila).
planta(zabila).
planta(anacahuite).  % Hierba LGGP 
planta(arnica).      % Hierba LGGP
planta(barbasco).    % Hierba LGGP
planta(epazote).     % Hierba RMD
planta(enebro).      % Hierba RMD
planta(estafiate).   % Hierba RMD

hierba('Manzanilla', ['calmante', 'antiinflamatorio', 'digestivo', 'antiespasmódico']).
hierba('Eucalipto', ['descongestionante', 'antiséptico', 'expectorante']).
hierba('Menta', ['analgésico', 'digestivo', 'descongestionante']).
hierba('Tila', ['sedante', 'calmante', 'antiespasmódico']).
hierba('Aloe Vera', ['antiséptico', 'cicatrizante', 'hidratante']).
hierba('Anacahuite', ['antiinflamatorio', 'antiséptico', 'expectorante']).
hierba('Árnica', ['antiinflamatorio', 'analgésico', 'cicatrizante']).
hierba('Barbasco', ['antiespasmódico', 'digestivo', 'diurético']).
hierba('Epazote', ['antiparasitario', 'digestivo', 'expectorante']).
hierba('Enebro', ['diurético', 'digestivo', 'antiséptico']).
hierba('Estafiate', ['antiparasitario', 'digestivo', 'antiinflamatorio']).


% Componentes de las plantas
componentes(manzanilla, [flavonoides, aceites_esenciales, cumarinas]).
componentes(menta, [mentol, flavonoides, aceites_esenciales]).
componentes(eucalipto, [eucaliptol, taninos, flavonoides]).
componentes(tila, [flavonoides, aceites_esenciales, taninos]).
componentes(zabila, [aloina, emodina, antraquinonas]).
componentes(anacahuite, [flavonoides, alcaloides, taninos]).
componentes(arnica, [sesquiterpenlactonas, aceites_esenciales, flavonoides]).
componentes(barbasco, [alcaloides, saponinas, taninos]).
componentes(epazote, [ascaridol, flavonoides, taninos]).
componentes(enebro, [aceites_esenciales, flavonoides, taninos]).
componentes(estafiate, [cineol, tuyona, flavonoides]).

% Medicamentos y plantas que los producen
produce(manzanilla, infusion).
produce(menta, infusion).
produce(eucalipto, jarabe).
produce(tila, infusion).
produce(zabila, gel).
produce(anacahuite, infusion).
produce(arnica, pomada).
produce(barbasco, tintura).
produce(epazote, infusion).
produce(enebro, infusion).
produce(estafiate, infusion).

% Efectos de los medicamentos provenientes de plantas
efecto(infusion, anacahuite, [antiinflamatorio, analgésico, diurético]).
efecto(pomada, arnica, [cicatrizante, antiinflamatorio, analgésico]).
efecto(tintura, barbasco, [antiespasmódico, digestivo, expectorante]).
efecto(infusion, epazote, [antiparasitario, digestivo, expectorante]).
efecto(infusion, enebro, [diurético, digestivo, antiséptico]).
efecto(infusion, estafiate, [antiparasitario, digestivo, antiinflamatorio]).

% Enfermedades que curan las plantas
cura(manzanilla, [indigestion, ansiedad, inflamacion]).
cura(menta, [indigestion, dolor_cabeza, resfriado]).
cura(eucalipto, [resfriado, tos, congestion]).
cura(tila, [ansiedad, insomnio, nerviosismo]).
cura(zabila, [quemaduras, heridas, irritaciones_piel]).
cura(anacahuite, [artritis, cistitis, dolor_muscular]).
cura(arnica, [contusiones, hematomas, esguinces]).
cura(barbasco, [asma, bronquitis, reumatismo]).
cura(epazote, [parasitosis, indigestión, resfriado]).
cura(enebro, [cistitis, indigestión, infecciones_urinarias]).
cura(estafiate, [parasitosis, indigestión, inflamación]).


% Formas de preparación
preparacion(manzanilla, infusion, 'Colocar las flores secas en agua caliente y dejar reposar 10 minutos.').
preparacion(menta, infusion, 'Colocar las hojas en agua caliente y dejar reposar 5 minutos.').
preparacion(eucalipto, jarabe, 'Hervir las hojas y mezclar con miel hasta obtener una consistencia de jarabe.').
preparacion(tila, infusion, 'Colocar las flores en agua caliente y dejar reposar 5-10 minutos.').
preparacion(zabila, gel, 'Extraer el gel de las hojas y aplicar directamente sobre la piel.').
preparacion(anacahuite, infusion, 'Hervir las hojas y dejar reposar 10 minutos antes de beber').
preparacion(arnica, pomada, 'Mezclar los extractos con cera de abejas y aceite hasta obtener una pomada').
preparacion(barbasco, tintura, 'Macerar las raices de barbasco en alcohol durante 1 semana, luego filtrar').
preparacion(epazote, infusion, 'Hervir las hojas y dejar reposar 10 minutos antes de beber').
preparacion(enebro, infusion, 'Hervir las bayas y dejar reposar 10 minutos antes de beber').
preparacion(estafiate, infusion, 'Hervir las hojas y dejar reposar 10 minutos antes de beber').


% Nombre científico de las plantas
nombre_cientifico(manzanilla, 'Matricaria chamomilla').
nombre_cientifico(menta, 'Mentha piperita').
nombre_cientifico(eucalipto, 'Eucalyptus globulus').
nombre_cientifico(tila, 'Tilia platyphyllos').
nombre_cientifico(zabila, 'Aloe vera').
nombre_cientifico(anacahuite, 'Cecropia peltata').
nombre_cientifico(arnica, 'Arnica montana').
nombre_cientifico(barbasco, 'Dioscorea trifida').
nombre_cientifico(epazote, 'Dysphania ambrosioides').
nombre_cientifico(enebro, 'Juniperus communis').
nombre_cientifico(estafiate, 'Artemisia ludoviciana').

% Orígenes de las plantas
origen(manzanilla, 'Europa y Asia').
origen(menta, 'Europa').
origen(eucalipto, 'Australia').
origen(tila, 'Europa').
origen(zabila, 'Africa').
origen(anacahuite, 'America Central').
origen(arnica, 'Europa').
origen(barbasco, 'Sudamerica').
origen(epazote, 'Mexico y America Central').
origen(enebro, 'Europa').
origen(estafiate, 'America del Norte').

% Inicio del programa
inicio :-
    new(Dialog, dialog('Sistema Experto "El Yerberito"')),
    send(Dialog, size, size(1085, 700)),  % Establecer el tamaño del diálogo
    send(Dialog, display, new(BG, bitmap('C:/Users/Infectedblood/Desktop/Yerberito/Imagenes/yerbe.jpg'))),  % Cargar la imagen de fondo
    send(BG, size, Dialog?size),  % Establecer el tamaño de la imagen igual al tamaño del diálogo
    send(BG, position, point(0, 0)),  % Establecer la posición de la imagen en la esquina superior izquierda
    send_list(Dialog, append,
              [ button('Plantas Medicinales', message(@prolog, mostrar_plantas)),
                button('Diagnosticar', message(@prolog, diagnosticar)),
                button('Salir', message(Dialog, destroy))
              ]),
    send(Dialog, open).

% Mostrar las plantas medicinales
mostrar_plantas :-
    new(Dialog, dialog('Plantas Medicinales')),
    send(Dialog, size, size(1180, 700)),  % Establecer el tamaño del diálogo
    send(Dialog, display, new(BG, bitmap('C:/Users/Infectedblood/Desktop/Yerberito/Imagenes/botiquin.jpg'))),  % Cargar la imagen de fondo
    send(BG, size, Dialog?size),  % Establecer el tamaño de la imagen igual al tamaño del diálogo
    send(BG, position, point(0, 0)),  % Establecer la posición de la imagen en la esquina superior izquierda
    send_list(Dialog, append,
              [ button('Manzanilla', message(@prolog, mostrar_planta, manzanilla)),
                button('Menta', message(@prolog, mostrar_planta, menta)),
                button('Eucalipto', message(@prolog, mostrar_planta, eucalipto)),
                button('Tila', message(@prolog, mostrar_planta, tila)),
                button('Zabila', message(@prolog, mostrar_planta, zabila)),
                button('Anacahuite', message(@prolog, mostrar_planta, anacahuite)), % Nuevo botón
                button('Árnica', message(@prolog, mostrar_planta, arnica)),           % Nuevo botón
                button('Barbasco', message(@prolog, mostrar_planta, barbasco)),       % Nuevo botón
                button('Epazote', message(@prolog, mostrar_planta, epazote)), % Nuevo botón
                button('Enebro', message(@prolog, mostrar_planta, enebro)),           % Nuevo botón
                button('Estafiate', message(@prolog, mostrar_planta, estafiate)),       % Nuevo botón
                % Agregar botones para las otras hierbas según sea necesario
                button('Regresar', message(Dialog, destroy))
              ]),
    send(Dialog, open).


% Mostrar información específica de una planta
mostrar_planta(Planta) :-
    componentes(Planta, Componentes),
    nombre_cientifico(Planta, NombreCientifico),
    origen(Planta, Origen),
    produce(Planta, Forma),
    preparacion(Planta, Forma, Preparacion),
    findall(Efecto, efecto(Forma, Planta, Efecto), Efectos),
    findall(Medicamento, produce(Planta, Medicamento), Medicamentos),
    findall(Accion, hierba(Planta, Accion), AccionesPlanta),
    (planta_medicinal(Planta) -> Medicinal = 'Si'; Medicinal = 'No'),
    new(Dialog, dialog('* Información de Planta')),
    send(Dialog, append, text('* Nombre Comun:')),
    send(Dialog, append, text(Planta)),
    send(Dialog, append, text('* Nombre Cientifico:')),
    send(Dialog, append, text(NombreCientifico)),
    send(Dialog, append, text('* Origen:')),
    send(Dialog, append, text(Origen)),
    send(Dialog, append, text('* Componentes:')),
    mostrar_lista(Dialog, Componentes),
    send(Dialog, append, text('* Forma de Preparacion:')),
    send(Dialog, append, text(Preparacion)),
    send(Dialog, append, text('* Medicinal:')),
    send(Dialog, append, text(Medicinal)),
    send(Dialog, append, button('Regresar', message(Dialog, destroy))),
    % Añadir imagen de la planta
    atom_concat('C:/Users/Infectedblood/Desktop/Yerberito/Imagenes/', Planta, ImagenPath1),
    atom_concat(ImagenPath1, '.jpg', ImagenPath),
    new(Imagen, bitmap(ImagenPath)),
    send(Dialog, display, Imagen, point(500, 10)),  % Ajustar la posición de la imagen
    send(Dialog, open).

% Mostrar lista de componentes o efectos
mostrar_lista(_, []).
mostrar_lista(Dialog, [Cabeza|Cola]) :-
    send(Dialog, append, text(Cabeza)),
    mostrar_lista(Dialog, Cola).

% Predicado para determinar si una planta es medicinal
planta_medicinal(Planta) :-
    planta(Planta).



% Obtener lista de plantas y sus acciones o efectos sobre el organismo
plantas_efectos(PlantasEfectos) :-
    findall((Planta, Efectos), (planta(Planta), efecto(_, Planta, Efectos)), PlantasEfectos).



% Obtener lista de plantas y sus acciones o efectos sobre el organismo
plantas_efectos(PlantasEfectos) :-
    findall((Planta, Efectos), (planta(Planta), efecto(_, Planta, Efectos)), PlantasEfectos).

% Mostrar lista de plantas y sus acciones o efectos sobre el organismo
mostrar_lista_efectos(_, []).
mostrar_lista_efectos(Dialog, [(Planta, Efectos)|Resto]) :-
    send(Dialog, append, text(Planta)),
    send(Dialog, append, text(': ')),
    % Convertir cada efecto en un objeto de texto
    maplist(atom_codes, Efectos, EfectosAtomCodes),
    maplist(string_codes, EfectosStrings, EfectosAtomCodes),
    maplist(text, EfectosTexts, EfectosStrings),
    % Enviar cada efecto al diálogo
    send_list(Dialog, append, EfectosTexts),
    % Recursivamente mostrar el resto de la lista
    mostrar_lista_efectos(Dialog, Resto).


% Diagnóstico
diagnosticar :-
    new(Dialog, dialog('Diagnostico')),
    send(Dialog, size, size(1180, 700)),  % Establecer el tamaño del diálogo
    send(Dialog, display, new(BG, bitmap('C:/Users/Infectedblood/Desktop/Yerberito/Imagenes/Diagnostico.jpg'))),  % Cargar la imagen de fondo
    send(BG, size, Dialog?size),  % Establecer el tamaño de la imagen igual al tamaño del diálogo
    send(BG, position, point(0, 0)),  % Establecer la posición de la imagen en la esquina superior izquierda
    send_list(Dialog, append,
              [ new(Sintoma, menu(sintoma, cycle)),
                button('Recomendar', message(@prolog, recomendar_planta, Sintoma?selection)),
                button('Regresar', message(Dialog, destroy))
              ]),
    send_list(Sintoma, append, [indigestion, ansiedad, inflamacion, dolor_cabeza, resfriado, tos, congestion, insomnio, nerviosismo, quemaduras, heridas, irritaciones_piel]),
    send(Dialog, open).

% Recomendar planta basada en el síntoma
recomendar_planta(Sintoma) :-
    findall(Planta, (cura(Planta, Sintomas), member(Sintoma, Sintomas)), Plantas),
    mostrar_recomendacion(Sintoma, Plantas).

% Mostrar recomendación
mostrar_recomendacion(Sintoma, Plantas) :-
    new(Dialog, dialog('Recomendacion')),
    send(Dialog, display, new(BG, bitmap('C:/Users/Infectedblood/Desktop/Yerberito/Imagenes/kokun.jpg'))),  % Cargar la imagen de fondo
    send(BG, size, Dialog?size),  % Establecer el tamaño de la imagen igual al tamaño del diálogo
    send(BG, position, point(100, 10)),  % Establecer la posición de la imagen en la esquina superior izquierda
    send(Dialog, append, text('Para el sintoma:')),
    send(Dialog, append, text(Sintoma)),
    send(Dialog, append, text('Se recomiendan las siguientes plantas:')),
    mostrar_lista(Dialog, Plantas),
    send(Dialog, append, text('Puede revisar nuestro apartado de plantas medicinales para saber mas al respecto.')),
    send(Dialog, append, button('Regresar', message(Dialog, destroy))),
    send(Dialog, open).

% Preguntas adicionales
plantas_medicinales(Plantas) :- findall(Planta, planta(Planta), Plantas).
plantas_producen_medicamentos(Plantas) :- findall(Planta, produce(Planta, _), Plantas).
medicamentos_provenientes_plantas(Medicamentos) :- findall(Medicamento, produce(_, Medicamento), Medicamentos).
enfermedades_curadas_plantas(Enfermedades) :- findall(Enfermedad, cura(_, Enfermedad), Enfermedades).
enfermedades_cura_planta(Planta, Enfermedades) :- cura(Planta, Enfermedades).
plantas_curan_enfermedad(Enfermedad, Plantas) :- findall(Planta, (cura(Planta, Enfermedades), member(Enfermedad, Enfermedades)), Plantas).
formas_preparacion(Preparaciones) :- findall(Forma, preparacion(_, Forma, _), Preparaciones).
formas_preparacion_planta(Planta, Preparacion) :- preparacion(Planta, _, Preparacion).
origen_plantas(Orígenes) :- findall(Origen, origen(_, Origen), Orígenes).
origen_planta(Planta, Origen) :- origen(Planta, Origen).
tratamiento_enfermedad(Enfermedad, Plantas, Medicamentos) :- plantas_curan_enfermedad(Enfermedad, Plantas), findall(Medicamento, (produce(Planta, Medicamento), member(Planta, Plantas)), Medicamentos).
