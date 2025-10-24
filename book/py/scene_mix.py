#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


# from scene import Scene
from scene_mixin import SceneMixin
from scene_classical_chess import SceneClassicalChessMixin
from scene_croatian_ties import SceneCroatianTiesMixin
from scene_mayan_ascendancy import SceneMayanAscendancyMixin
from scene_age_of_aquarius import SceneAgeOfAquariusMixin
from scene_mirandas_veil import SceneMirandasVeilMixin
from scene_nineteen import SceneNineteenMixin
from scene_hemeras_dawn import SceneHemerasDawnMixin
from scene_tamoanchan_revisited import SceneTamoanchanRevisitedMixin
from scene_conquest_of_tlalocan import SceneConquestOfTlalocanMixin
from scene_discovery import SceneDiscoveryMixin
from scene_one import SceneOneMixin
from scene_simple import SceneSimpleMixin


class SceneMix( SceneMixin, \
                SceneClassicalChessMixin, \
                SceneCroatianTiesMixin, \
                SceneMayanAscendancyMixin, \
                SceneAgeOfAquariusMixin, \
                SceneMirandasVeilMixin, \
                SceneNineteenMixin, \
                SceneHemerasDawnMixin, \
                SceneTamoanchanRevisitedMixin, \
                SceneConquestOfTlalocanMixin, \
                SceneDiscoveryMixin, \
                SceneOneMixin, \
                SceneSimpleMixin ):

    # overrides
    def _get_recent_scene_method_names( self ):
        return  [
                    'scn_n_16_sideways_pawn', \
                    # 'scn_n_17_sideways_pawn_cannot_activate_pyramid', \
                    # 'scn_n_18_sideways_pawn_can_activate_wave', \
                    # # 'scn_n_16_sideways_pawn_init', \
                    'scn_n_19_sideways_pawn_activated_wave', \
                    # 'scn_n_18_capture_pawn_init', \
                    # 'scn_n_19_capture_pawn_activated_wave', \
                    # 'scn_n_20_activating_opponents_wave', \
                    # 'scn_n_21_activated_opponents_wave', \
                    # 'scn_n_22_sideways_pawns_and_activating_pyramid', \
                ]
