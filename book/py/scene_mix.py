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
                SceneOneMixin ):

    # overrides
    def _get_recent_scene_method_names( self ):
        return  [
                    'scn_tr_09_serpent_loop_init', \
                    'scn_tr_10_serpent_loop_step', \
                    'scn_tr_11_serpent_loop_end', \
                    'scn_cot_030_own_shaman_is_divergent_init', \
                    'scn_cot_031_own_shaman_is_divergent_end', \
                    'scn_cot_052_wave_divergence_init', \
                    'scn_o_30_starchild_divergence_init', \
                    # 'scn_o_31_starchild_divergence_end',
                ]
